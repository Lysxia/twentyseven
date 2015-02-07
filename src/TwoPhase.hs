{- | Two phase algorithm to solve a Rubik's cube -}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor,
             MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             TemplateHaskell #-}
module TwoPhase (
  twoPhase,

  twoPhaseTables,

  -- * Phase 1
  phase1,
  phase1',
  phase1Solved,
  Phase1Coord (..),
  phase1Move18,
  phase1Dist,

  -- * Phase 2
  phase2,
  phase2',
  phase2Solved,
  Phase2Coord (..),
  phase2Move10,
  phase2Dist,

  -- * Strict tuples
  Tuple2 (..),
  Tuple3 (..),
  )
  where

import Prelude hiding ( maximum )

import Coord
import Cubie
import Distances
import IDA
import Moves
import Misc
import StrictTuples

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Foldable ( Foldable, maximum )
import Data.Function ( on )
import Data.List hiding ( maximum )
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

$(decTuple 2)
$(decTuple 3)

instance NFData a => NFData (Tuple2 a) where
  rnf (Tuple2 a a') = rnf a `seq` rnf a' `seq` ()

transposeTuple2 :: Tuple2 [a] -> [Tuple2 a]
{-# INLINE transposeTuple2 #-}
transposeTuple2 (Tuple2 as as') = zipWith Tuple2 as as'

instance NFData a => NFData (Tuple3 a) where
  rnf (Tuple3 a a' a'') = rnf a `seq` rnf a' `seq` rnf a'' `seq` ()

transposeTuple3 :: Tuple3 [a] -> [Tuple3 a]
{-# INLINE transposeTuple3 #-}
transposeTuple3 (Tuple3 as as' as'') = zipWith3 Tuple3 as as' as''

-- | Phase 1 coordinate representation, a /pair/ (length-2 list)
-- representing:
--
-- - UD slice edge positions and edge orientations
-- - Corner orientations.
newtype Phase1Coord = Phase1Coord { phase1Unwrap :: Tuple2 Int }
  deriving Eq

move18Coord :: CA a -> Coordinate a -> [Vector Coord]
move18Coord ca coord = moveTable ca coord <$> move18

-- | Move tables
phase1Move18 :: [Tuple2 (Vector Coord)]
phase1Move18 = zipWith Tuple2
  (move18Coord (CA2 CA1 CA1) coordFlipUDSlice)
  (move18Coord CA1 coordCornerOrien)

-- | Pruning tables
phase1Dist :: Tuple2 (Vector Int)
phase1Dist
  = zipWith' tableToDistance
    (phase1Unwrap . phase1Encode $ iden)
    (sequence' phase1Move18)

-- | Phase 1 uses @FlipUDSlice@ and @CornerOrien@.
phase1Encode :: Cube -> Phase1Coord
phase1Encode = Phase1Coord . (<*>) (Tuple2
    (encode coordFlipUDSlice . fromCube) -- Cannot factor @fromCube@:
    (encode coordCornerOrien . fromCube) -- different instances involved
  ) . pure

type Phase1Opt = (Int, Phase1Coord)

-- | ==Branching reduction
--
-- The @Int@ projection keeps track of the latest move (@== 6@
-- for the starting point).
--
-- We can indeed reduce the branching factor from 18 to 15
-- by considering that successive moves on the same face
-- can and will be shortened as a single move.
--
-- Furthermore, since moves on opposite faces commute, we may force
-- them to be in an arbitrary order, reducing the branching factor
-- to 12 after half of the moves (U, L, F).
--
-- ==Experiments
--
-- Some samples show that phase 1 takes negligible time compared to phase 2
-- in average.
--
-- Yet in both, an acceleration can be observed,
-- with a speed-up factor going as high as 10.
--
phase1Search :: Search Int ElemMove Phase1Opt
phase1Search = Search {
    goal = (== phase1Encode iden) . snd,
    estm = maximum . zipWith' (U.!) phase1Dist . phase1Unwrap . snd,
    edges = \(i, x)
      -> [ Succ l 1 (j, Phase1Coord $ zipWith' (U.!) ms (phase1Unwrap x))
         | (l, j, ms) <- succVector V.! i ]
  }
  where
    succVector
      = V.snoc
          (V.generate 6 $ \i ->
            [ m | m@(_, j, _) <- moves,
              not (i == j || oppositeAndGT (toEnum j) (toEnum i)) ])
          moves
    moves = zip3 move18Names (fromEnum . head <$> move18Names) phase1Move18

-- | Without the branching reduction (for comparison purposes)
phase1Search' :: Search Int ElemMove Phase1Coord
phase1Search' = Search {
    goal = (== phase1Encode iden),
    estm = maximum . zipWith' (U.!) phase1Dist . phase1Unwrap,
    edges = zipWith (Succ `flip` 1) move18Names
            . (Phase1Coord <$>)
            . (zipWith' (U.!) <$> phase1Move18 <*>)
            . pure . phase1Unwrap
  }
  

-- | Phase 1: reduce to \<U, D, L2, F2, R2, B2\>.
phase1 :: Cube -> Move
phase1 = head . snd . fromJust . first . search phase1Search
    . (\x -> (6 :: Int, x))
    . phase1Encode

phase1' :: Cube -> Move
phase1' = head . snd . fromJust . first . search phase1Search' . phase1Encode

-- | > phase1Solved (phase1 c)
phase1Solved :: Cube -> Bool
phase1Solved = ((==) `on` phase1Encode) iden

--

-- | Phase 2 coordinate representation, a /triple/ (length-3 list)
-- representing:
--
-- - UD slice edge permutation
-- - Non-UD-slice edge permutation
-- - Corner permutation
newtype Phase2Coord = Phase2Coord { phase2Unwrap :: Tuple3 Int }
  deriving Eq

-- | Phase 2 does not use any product type.
move10Coord :: CubeAction a => Coordinate a -> [Vector Coord]
move10Coord coord = moveTable CA1 coord <$> move10

phase2Move10 :: [Tuple3 (Vector Int)]
phase2Move10 = zipWith3 Tuple3
  (move10Coord coordUDSlicePermu)
  (move10Coord coordUDEdgePermu)
  (move10Coord coordCornerPermu)

phase2Dist :: Tuple3 (Vector Int)
phase2Dist = zipWith' tableToDistance
  (phase2Unwrap . phase2Encode $ iden)
  (sequence' phase2Move10)

phase2Encode :: Cube -> Phase2Coord
phase2Encode = Phase2Coord . (<*>) (Tuple3
    (encode coordUDSlicePermu . fromCube)
    (encode coordUDEdgePermu  . fromCube)
    (encode coordCornerPermu  . fromCube)
  ) . pure

type Phase2Opt = (Int, Phase2Coord)
-- | Uses branching reduction
--
-- Instead of a factor 10, we have factors
--
-- - 9 after R, B;
-- - 8 after L, F;
-- - 7 after D;
-- - 4 after U.
--
--
phase2Search :: Search Int ElemMove Phase2Opt
phase2Search = Search {
    goal = (== phase2Encode iden) . snd,
    estm = maximum . zipWith' (U.!) phase2Dist . phase2Unwrap . snd,
    edges
      = \(i, x) -> do
        (l, j, ms) <- succVector V.! i
        return $
          Succ l 1 (j, Phase2Coord $ zipWith' (U.!) ms (phase2Unwrap x))
  }
  where
    succVector
      = V.snoc
          (V.generate 6 $ \i ->
            [ m | m@(_, j, _) <- moves,
              not (i == j || oppositeAndGT (toEnum j) (toEnum i)) ])
          moves
    moves = zip3 move10Names (fromEnum . head <$> move10Names) phase2Move10

phase2Search' :: Search Int ElemMove Phase2Coord
phase2Search' = Search {
    goal = (== phase2Encode iden),
    estm = maximum . zipWith' (U.!) phase2Dist . phase2Unwrap,
    edges = zipWith (flip Succ 1) move10Names
            . (Phase2Coord <$>)
            . (zipWith' (U.!) <$> phase2Move10 <*>)
            . pure . phase2Unwrap
  }

-- | Phase 2: solve a cube in \<U, D, L2, F2, R2, B2\>.
phase2 :: Cube -> Move
phase2 = head . snd . fromJust . first . search phase2Search
    . (\x -> (6 :: Int, x))
    . phase2Encode

phase2' :: Cube -> Move
phase2' = head . snd . fromJust . first . search phase2Search' . phase2Encode

-- | > phase1Solved c ==> phase2Solved (phase2 c)
phase2Solved :: Cube -> Bool
phase2Solved = (== iden)

--

-- | Solve a scrambled Rubik's cube.
--
-- Make sure the cube is actually solvable with 'Cubie.solvable',
-- before calling this function.
twoPhase :: Cube -> Move
twoPhase c
  = let s1 = phase1 c
        c1 = c <> moveToCube s1
        s2 = phase2 c1
    in reduceMove $ s1 ++ s2

-- | Strict in the move tables and distance tables:
--
-- - 'phase1Move18'
-- - 'phase1Dist'
-- - 'phase2Move10'
-- - 'phase2Dist'
twoPhaseTables
  = phase1Move18 `deepseq`
    phase1Dist `deepseq`
    phase2Move10 `deepseq`
    phase2Dist `deepseq`
    ()

--

tableToDistance :: Coord -> [Vector Coord] -> Vector Int
tableToDistance root vs = distances (U.length (head vs)) root neighbors
  where neighbors = liftA2 (U.!) vs . pure

