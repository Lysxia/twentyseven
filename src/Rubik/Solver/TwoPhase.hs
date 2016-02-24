{- | Two phase algorithm to solve a Rubik's cube -}

{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Rubik.Solver.TwoPhase where

import Rubik.Cube
import Rubik.IDA
import Rubik.Misc
import Rubik.Solver
import Rubik.Tables.Moves
import Rubik.Tables.Distances

import Data.Function ( on )
import Data.Maybe
import Data.Monoid
import Data.StrictTuple

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

{-# INLINE phase1Proj #-}
phase1Proj
  =   rawProjection
  |:| rawProjection
  |.| rawProjection

phase1Convert = convertP phase1Proj

phase1Dist = maxDistance
  [ (\(Tuple3 co _ uds) -> (co, uds)) >$< distanceWith2 d_CornerOrien_UDSlice
  , (\(Tuple3 _ eo uds) -> (eo, uds)) >$< distanceWith2 d_EdgeOrien_UDSlice
  ]

phase1 :: Cube -> Move
phase1 =
    let moves = Tuple3 move18CornerOrien move18EdgeOrien move18UDSlice
        phase1Search = mkSearch move18Names moves phase1Proj phase1Dist
    in fromJust . search phase1Search . tag . phase1Convert

-- | > phase1Solved (phase1 c)
phase1Solved :: Cube -> Bool
phase1Solved = ((==) `on` phase1Convert) iden

--

phase2Proj
  =   rawProjection
  |:| rawProjection
  |.| rawProjection

phase2Convert = convertP phase2Proj

phase2Dist = maxDistance
  [ (\(Tuple3 cp _ udsp) -> (cp, udsp)) >$< distanceWith2 d_CornerPermu_UDSlicePermu2
  , (\(Tuple3 _ udep udsp) -> (udep, udsp)) >$< distanceWith2 d_UDEdgePermu2_UDSlicePermu2
  ]

phase2 :: Cube -> Move
phase2 =
    let moves = Tuple3 move10CornerPermu move10UDEdgePermu2 move10UDSlicePermu2
        phase2Search = mkSearch move10Names moves phase2Proj phase2Dist
    in fromJust . search phase2Search . tag . phase2Convert

-- | > phase1Solved c ==> phase2Solved (phase2 c)
phase2Solved :: Cube -> Bool
phase2Solved = (== iden)

-- | Solve a scrambled Rubik's cube.
--
-- Make sure the cube is actually solvable with 'Cubie.solvable',
-- before calling this function.
twoPhase :: Cube -> Move
twoPhase c =
  let s1 = phase1 c
      c1 = c <> moveToCube s1
      s2 = phase2 c1
  in reduceMove $ s1 ++ s2
