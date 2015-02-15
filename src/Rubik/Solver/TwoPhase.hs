{- | Two phase algorithm to solve a Rubik's cube -}

{-# LANGUAGE ViewPatterns #-}
module Rubik.Solver.TwoPhase (
  twoPhase,

  -- * Phase 1
  Phase1Coord,
  Phase1Distances,
  phase1,
  phase1Solved,
  phase1DistTables,

  -- * Phase 2
  Phase2Coord,
  Phase2Distances,
  phase2,
  phase2Solved,
  phase2DistTables,
  ) where

import Rubik.Cube
import Rubik.IDA
import Rubik.Misc
import Rubik.Symmetry
import Rubik.Solver

import Control.Applicative
import Control.Monad

import Data.Binary.Store
import Data.Function ( on )
import Data.List hiding ( maximum )
import Data.Maybe
import Data.Monoid
import Data.StrictTuple
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import System.FilePath

-- | Phase 1 coordinate representation, a /pair/ (length-2 list)
-- representing:
--
-- - UD slice edge positions and edge orientations
-- - Corner orientations.
type Phase1Coord = Tuple3 Int

-- | Phase 2 coordinate representation, a /triple/ (length-3 list)
-- representing:
--
-- - UD slice edge permutation
-- - Non-UD-slice edge permutation
-- - Corner permutation
type Phase2Coord = Tuple3 Int

-- | Length 2 list
type Phase1Distances = [Vector DInt]

type Phase2Distances = [Vector DInt]

phase1 :: Phase1Distances -> Cube -> Move
phase1 dist = extract . search (phase1SearchWithD dist) . phase1Encode

phase2 :: Phase2Distances -> Cube -> Move
phase2 dist = extract . search (phase2SearchWithD dist) . phase2Encode

phase1SearchWithD :: Phase1Distances -> Search V.Vector DInt ElemMove (Tag Phase1Coord)
phase1SearchWithD = searchWith move18Names phase1CI transpose3 . flip zip phase1DistIndices

phase2SearchWithD :: Phase2Distances -> Search V.Vector DInt ElemMove (Tag Phase2Coord)
phase2SearchWithD = searchWith move10Names phase2CI transpose3 . flip zip phase2DistIndices

phase1CI = Tuple3 move18CornerOrien move18EdgeOrien move18UDSlice

phase2CI = Tuple3 move10CornerPermu move10UDEdgePermu2 move10UDSlicePermu2

phase1Encode = encodeCI' phase1CI

phase2Encode = encodeCI' phase2CI

phase1DistTables  = [ dist_CornerOrien_UDSlice, dist_EdgeOrien_UDSlice ]
phase1DistIndices = [ Two rUDS (co, uds),       Two rUDS (eo, uds) ]
  where
    rUDS = range coordUDSlice
    [co, eo, uds] = [0 .. 2]

phase1Dist :: FilePath -> IO Phase1Distances
phase1Dist path = mapM (retrieve path) phase1DistTables

phase2DistTables  = [ dist_CornerPermu_UDSlicePermu2, dist_EdgePermu2 ]
phase2DistIndices = [ Two rUDSP (cp, udsp),           Two rUDSP (ude, udsp) ]
  where
    rUDSP = range coordUDSlicePermu2
    [cp, ude, udsp] = [0 .. 2]

phase2Dist :: FilePath -> IO Phase2Distances
phase2Dist path = mapM (retrieve path) phase2DistTables

phase1SearchWithD' :: Phase2Distances -> Search [] DInt ElemMove (Tag Phase1Coord)
phase1SearchWithD' [d_EO_UDS, d_CO_UDS] = Search {
    goal = goalSearch phase1CI,
    estm = \(_, Tuple3 co eo uds)
      -> max (d_EO_UDS U.! flatIndex rUDS eo uds)
             (d_CO_UDS U.! flatIndex rUDS co uds),
    edges = \(i, x)
      -> [ Succ l 1 (j, zipWith' (U.!) ms x) | (l, j, ms) <- succVector V.! i ]
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

-- | > phase1Solved (phase1 c)
phase1Solved :: Cube -> Bool
phase1Solved = ((==) `on` phase1Encode) iden

--

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
phase2Search' :: Phase2Distances -> Search V.Vector DInt ElemMove (Tag Phase2Coord)
phase2Search' [d_EP2, d_CP_UDSP2] = Search {
    goal = goalSearch phase2CI,
    estm = \(_, Tuple3 cp ep uds)
      -> max (d_EP2 U.! flatIndex rUDSP ep uds)
             (d_CP_UDSP2 U.! flatIndex rUDSP cp uds),
    edges
      = \(i, x) -> V.map (\(l, j, ms) -> let z = zipWith' (U.!) ms x in
          z `seq` Succ l 1 (j, z))
        (succVector V.! i)
  }
  where
    rUDSP = range coordUDSlicePermu2
    succVector
      = V.snoc
          (V.generate 6 $ \i -> V.filter
            (\(_, j, _) ->
              not (i == j || oppositeAndGT (toEnum j) (toEnum i)))
            moves)
          moves
    moves = V.fromList $ zip3 move10Names (fromEnum . snd <$> move10Names)
        (transpose3 $ movesCI <$> phase2CI)

-- | > phase1Solved c ==> phase2Solved (phase2 c)
phase2Solved :: Cube -> Bool
phase2Solved = (== iden)

--

-- | Solve a scrambled Rubik's cube.
--
-- Make sure the cube is actually solvable with 'Cubie.solvable',
-- before calling this function.
twoPhase :: Phase1Distances -> Phase2Distances -> Cube -> Move
twoPhase dist1 dist2 c
  = let s1 = phase1 dist1 c
        c1 = c <> moveToCube s1
        s2 = phase2 dist2 c1
    in reduceMove $ s1 ++ s2

