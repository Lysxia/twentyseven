{- | Two phase algorithm to solve a Rubik's cube -}

{-# LANGUAGE ViewPatterns #-}
module Solver.TwoPhase (
  twoPhase,

  twoPhaseTables,

  -- * Phase 1
  phase1,
  phase1',
  phase1Solved,
  Phase1Coord (..),

  -- * Phase 2
  phase2,
  phase2',
  phase2Solved,
  Phase2Coord (..),
  ) where

import Coord
import Cubie
import IDA
import Moves
import Misc
import Solver
import StrictTuple
import Symmetry

import Control.Applicative
import Control.Monad

import Data.Function ( on )
import Data.List hiding ( maximum )
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

phase1 = extract . search phase1Search . encodeCI' phase1CI
  where
    phase1Search = searchWith move18Names phase1CI transpose3 phase1Dist

phase2 = extract . search phase2Search . encodeCI' phase2CI
  where
    phase2Search = searchWith move10Names phase2CI transpose3 phase2Dist

-- ** Phase 1
phase1CI = Tuple3 move18CornerOrien move18EdgeOrien move18UDSlice

-- ** Phase 2
phase2CI = Tuple3 move10CornerPermu move10UDEdgePermu2 move10UDSlicePermu2

phase1Dist =
  [ (dist_CornerOrien_UDSlice, Two rUDS (co, uds)),
    (dist_EdgeOrien_UDSlice, Two rUDS (eo, uds)) ]
  where
    rUDS = range coordUDSlice
    [co, eo, uds] = [0 .. 2]

phase2Dist =
  [ (dist_CornerPermu_UDSlicePermu2, Two rUDSP (cp, udsp)),
    (dist_EdgePermu2, Two rUDSP (ude, udsp)) ]
  where
    rUDSP = range coordUDSlicePermu2
    [cp, ude, udsp] = [0 .. 2]

-- | Phase 1 coordinate representation, a /pair/ (length-2 list)
-- representing:
--
-- - UD slice edge positions and edge orientations
-- - Corner orientations.
newtype Phase1Coord = Phase1Coord { phase1Unwrap :: Tuple3 Int }
  deriving Eq

-- | Phase 1 uses @EdgeOrien_UDSlice@ and @CornerOrien@.
phase1Encode :: Cube -> Phase1Coord
phase1Encode = Phase1Coord . (encodeCI <$> phase1CI <*>) . pure

type Phase1Opt = (Int, Phase1Coord)

phase1Search' :: Search DInt ElemMove Phase1Opt
phase1Search' = Search {
    goal = (== phase1Encode iden) . snd,
    estm = \(_, Phase1Coord (Tuple3 co eo uds))
      -> max (dist_EdgeOrien_UDSlice U.! flatIndex rUDS eo uds)
             (dist_CornerOrien_UDSlice U.! flatIndex rUDS co uds),
    edges = \(i, x)
      -> [ Succ l 1 (j, Phase1Coord $ zipWith' (U.!) ms (phase1Unwrap x))
         | (l, j, ms) <- succVector V.! i ]
  }
  where
    rUDS = range coordUDSlice
    succVector
      = V.snoc
          (V.generate 6 $ \i ->
            [ m | m@(_, j, _) <- moves,
              not (i == j || oppositeAndGT (toEnum j) (toEnum i)) ])
          moves
    moves = zip3 move18Names (fromEnum . snd <$> move18Names)
        (transpose3 $ movesCI <$> phase1CI)

-- | Phase 1: reduce to \<U, D, L2, F2, R2, B2\>.
phase1' :: Cube -> Move
phase1' = extract . search phase1Search' . (,) 6 . phase1Encode

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

phase2Encode :: Cube -> Phase2Coord
phase2Encode = Phase2Coord . (encodeCI <$> phase2CI <*>) . pure

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
    estm = \(_, Phase2Coord (Tuple3 cp ep uds))
      -> max (dist_EdgePermu2 U.! flatIndex rUDSP ep uds)
             (dist_CornerPermu_UDSlicePermu2 U.! flatIndex rUDSP cp uds),
    edges
      = \(i, x) -> do
        (l, j, ms) <- succVector V.! i
        return $
          Succ l 1 (j, Phase2Coord $ zipWith' (U.!) ms (phase2Unwrap x))
  }
  where
    rUDSP = range coordUDSlicePermu2
    succVector
      = V.snoc
          (V.generate 6 $ \i ->
            [ m | m@(_, j, _) <- moves,
              not (i == j || oppositeAndGT (toEnum j) (toEnum i)) ])
          moves
    moves = zip3 move10Names (fromEnum . snd <$> move10Names)
        (transpose3 $ movesCI <$> phase2CI)

-- | Phase 2: solve a cube in \<U, D, L2, F2, R2, B2\>.
phase2' :: Cube -> Move
phase2' = extract . search phase2Search' . (,) 6 . phase2Encode

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

-- | Strict in the move tables and distance tables:
--
-- - 'phase1Move18'
-- - 'phase1Dist'
-- - 'phase2Move10'
-- - 'phase2Dist'
twoPhaseTables
  = (listSeq <$> movesCI <$> phase1CI) `seq`
    dist_EdgeOrien_UDSlice `seq` dist_CornerOrien_UDSlice `seq`
    (listSeq <$> movesCI <$> phase2CI) `seq`
    dist_EdgePermu2 `seq` dist_CornerPermu_UDSlicePermu2 `seq`
    ()

