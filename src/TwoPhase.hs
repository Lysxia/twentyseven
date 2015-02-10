{- | Two phase algorithm to solve a Rubik's cube -}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor,
             MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             TemplateHaskell, ViewPatterns #-}
module TwoPhase (
  twoPhase,

  twoPhaseTables,

  -- * Phase 1
  phase1,
  phase1',
  phase1Solved,
  Phase1Coord (..),
  phase1Move18,

  -- * Phase 2
  phase2,
  phase2',
  phase2Solved,
  Phase2Coord (..),
  phase2Move10,

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
import Symmetry

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.Foldable ( Foldable, maximum, toList )
import Data.Function ( on )
import Data.Int ( Int8 )
import Data.List hiding ( maximum )
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

$(decTuple 2)
$(decTuple 3)

transposeTuple2 :: Tuple2 [a] -> [Tuple2 a]
{-# INLINE transposeTuple2 #-}
transposeTuple2 (Tuple2 as as') = zipWith Tuple2 as as'

transposeTuple3 :: Tuple3 [a] -> [Tuple3 a]
{-# INLINE transposeTuple3 #-}
transposeTuple3 (Tuple3 as as' as'') = zipWith3 Tuple3 as as' as''

type DInt = Int8
-- | Phase 1 coordinate representation, a /pair/ (length-2 list)
-- representing:

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
searchWith
  :: (Applicative f, Foldable f, Eq (f Coord))
  => [ElemMove]
  -> f CoordInfo
  -> (f [Vector Coord] -> [f (Vector Coord)])
  -> [DistParam]
  -> Search DInt ElemMove (Int, f Int)
{-# INLINE searchWith #-}
searchWith moveNames ci transpose dists = Search goal estm edges
  where
    goal = let g1 = encodeCI <$> ci <*> pure iden in (g1 ==) . snd
    estm (_, t) = maximum $ estm' <$> dists <*> pure t
    estm' (d, One x) = \t -> d U.! (t ? x)
    estm' (d, Two dim (x, y)) = \t -> d U.! flatIndex dim (t ? x) (t ? y)
    edges (i, t)
      = [ x `seq` Succ l 1 (fromEnum j, x)
        | (l@(_, j), succs) <- succVector V.! i, let x = (U.!) <$> succs <*> t]
    succVector
      = V.snoc
          (V.generate 6 $ \(toEnum -> i) ->
            [ m | m@((_, j), _) <- moves,
              not (i == j || oppositeAndGT j i) ])
          moves
    moves = zip moveNames . transpose $ movesCI <$> ci
    (?) = (!!) . toList

encodeSearch ci = (,) 6 . (<$> ci) . flip encodeCI
extractSearch = head . snd . fromJust . IDA.first

phase1 = extractSearch . search phase1Search . encodeSearch phase1CI
  where
    phase1Search = searchWith move18Names phase1CI transposeTuple3 phase1Dist

phase2 = extractSearch . search phase2Search . encodeSearch phase2CI
  where
    phase2Search = searchWith move10Names phase2CI transposeTuple3 phase2Dist

-- * Move tables
data CoordInfo = CoordInfo {
    movesCI :: [Vector Coord],
    encodeCI :: Cube -> Coord
  }

table :: CubeAction a => [Cube] -> Coordinate a -> [Vector Coord]
table move coord = moveTable coord <$> move

coordInfo :: FromCube a => [Vector Coord] -> Coordinate a -> CoordInfo
coordInfo moves coord = CoordInfo moves (encode coord . fromCube)

-- ** Phase 1
move18UDSlice = table move18 coordUDSlice
move18EdgeOrien = table move18 coordEdgeOrien
move18CornerOrien = table move18 coordCornerOrien

phase1CI = Tuple3 co eo uds
  where
    co = coordInfo move18CornerOrien coordCornerOrien
    eo = coordInfo move18EdgeOrien coordEdgeOrien
    uds = coordInfo move18UDSlice coordUDSlice

-- ** Phase 2
move10UDSlicePermu2 = table move10 coordUDSlicePermu2
move10UDEdgePermu2 = table move10 coordUDEdgePermu2
move10CornerPermu = table move10 coordCornerPermu

phase2CI = Tuple3 cp ude uds
  where
    cp = coordInfo move10CornerPermu coordCornerPermu
    ude = coordInfo move10UDEdgePermu2 coordUDEdgePermu2
    uds = coordInfo move10UDSlicePermu2 coordUDSlicePermu2

-- * Pruning tables
type DistParam = (Vector DInt, DistIndexType)

data DistIndexType
  = One { pos1 :: Int }
  | Two { dim2 :: Int, pos2 :: (Int, Int) }

-- ** Phase 1
dist_CO_UDS = distanceWithVec2
    coordCornerOrien move18CornerOrien
    coordUDSlice move18UDSlice

distFlipUDSlice = distanceWithVec2
    coordEdgeOrien move18EdgeOrien
    coordUDSlice move18UDSlice

phase1Dist =
  [ (dist_CO_UDS, Two rUDS (co, uds)),
    (distFlipUDSlice, Two rUDS (eo, uds)) ]
  where
    rUDS = range coordUDSlice
    [co, eo, uds] = [0 .. 2]

-- ** Phase 2
distEdgePermu2 = distanceWithVec2
    coordUDEdgePermu2 move10UDEdgePermu2
    coordUDSlicePermu2 move10UDSlicePermu2

dist_CP_UDSP = distanceWithVec2
    coordCornerPermu move10CornerPermu
    coordUDSlicePermu2 move10UDSlicePermu2

phase2Dist =
  [ (dist_CP_UDSP, Two rUDSP (cp, udsp)),
    (distEdgePermu2, Two rUDSP (ude, udsp)) ]
  where
    rUDSP = range coordUDSlicePermu2
    [cp, ude, udsp] = [0 .. 2]

-- ** Other
distCornerOrien = distanceWithVec coordCornerOrien move18CornerOrien
distEdgeOrien = distanceWithVec coordEdgeOrien move18EdgeOrien

flatIndex :: Int -> Int -> Int -> Int
flatIndex n x y = x * n + y

-- - UD slice edge positions and edge orientations
-- - Corner orientations.
newtype Phase1Coord = Phase1Coord { phase1Unwrap :: Tuple3 Int }
  deriving Eq

phase1Move18 :: [Tuple3 (Vector Int)]
phase1Move18 = zipWith3 Tuple3 move18UDSlice move18EdgeOrien move18CornerOrien

-- | Phase 1 uses @FlipUDSlice@ and @CornerOrien@.
phase1Encode :: Cube -> Phase1Coord
phase1Encode = Phase1Coord . (<*>) (Tuple3
    (encode coordUDSlice . fromCube) -- Cannot factor @fromCube@:
    (encode coordEdgeOrien . fromCube) -- different instances involved
    (encode coordCornerOrien . fromCube)
  ) . pure

type Phase1Opt = (Int, Phase1Coord)

phase1Search' :: Search DInt ElemMove Phase1Opt
phase1Search' = Search {
    goal = (== phase1Encode iden) . snd,
    estm = \(_, Phase1Coord (Tuple3 uds eo co))
      -> max (distFlipUDSlice U.! flatIndex (range coordUDSlice) eo uds)
             (dist_CO_UDS U.! flatIndex (range coordUDSlice) co uds),
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
    moves = zip3 move18Names (fromEnum . snd <$> move18Names) phase1Move18


-- | Phase 1: reduce to \<U, D, L2, F2, R2, B2\>.
phase1' :: Cube -> Move
phase1' = extractSearch . search phase1Search' . (,) 6 . phase1Encode

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

phase2Move10 :: [Tuple3 (Vector Int)]
phase2Move10 = zipWith3 Tuple3
    move10UDSlicePermu2
    move10UDEdgePermu2
    move10CornerPermu

phase2Encode :: Cube -> Phase2Coord
phase2Encode = Phase2Coord . (<*>) (Tuple3
    (encode coordUDSlicePermu2 . fromCube)
    (encode coordUDEdgePermu2  . fromCube)
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
phase2Search' :: Search DInt ElemMove Phase2Opt
phase2Search' = Search {
    goal = (== phase2Encode iden) . snd,
    estm = \(_, Phase2Coord (Tuple3 uds ep cp))
      -> max (distEdgePermu2 U.! flatIndex (range coordUDSlicePermu2) ep uds)
             (dist_CP_UDSP U.! flatIndex (range coordUDSlicePermu2) cp uds),
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
    moves = zip3 move10Names (fromEnum . snd <$> move10Names) phase2Move10

-- | Phase 2: solve a cube in \<U, D, L2, F2, R2, B2\>.
phase2' :: Cube -> Move
phase2' = extractSearch . search phase2Search' . (,) 6 . phase2Encode

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
        s2 = phase2' c1
    in reduceMove $ s1 ++ s2

listSeq :: [a] -> b -> b
listSeq [] b = b
listSeq (a : as) b = a `seq` listSeq as b

-- | Strict in the move tables and distance tables:
--
-- - 'phase1Move18'
-- - 'phase1Dist'
-- - 'phase2Move10'
-- - 'phase2Dist'
twoPhaseTables
  = phase1Move18 `listSeq`
    distFlipUDSlice `seq` dist_CO_UDS `seq`
    phase2Move10 `listSeq`
    distEdgePermu2 `seq` dist_CP_UDSP `seq`
    ()

--

coordFromCube :: FromCube a => Coordinate a -> Cube -> Coord
coordFromCube c = encode c . fromCube

distanceWithVec :: FromCube a => Coordinate a -> [Vector Coord] -> Vector DInt
distanceWithVec c vs = distances (U.length (head vs)) root neighbors
  where
    root = coordFromCube c iden
    neighbors = liftA2 (U.!) vs . pure

distanceWithVec2
  :: (FromCube a, FromCube b)
  => Coordinate a -> [Vector Coord]
  -> Coordinate b -> [Vector Coord] -> Vector DInt
distanceWithVec2 c1 vs1 c2 vs2 = distances n root neighbors
  where
    n = U.length (head vs1) * n2
    n2 = U.length (head vs2)
    root = flatIndex n2 (coordFromCube c1 iden) (coordFromCube c2 iden)
    neighbors ((`divMod` n2) -> (x1, x2))
      = zipWith (flatIndex n2)
          ((U.! x1) <$> vs1)
          ((U.! x2) <$> vs2)

