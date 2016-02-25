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

{-# INLINE phase1Proj #-}
phase1Proj
  =   rawProjection
  |*| rawProjection
  |.| rawProjection

phase1Convert = convertP phase1Proj

phase1Dist = maxDistance
  [ (\((,,) co _ uds) -> (co, uds)) >$< distanceWith2 d_CornerOrien_UDSlice
  , (\((,,) _ eo uds) -> (eo, uds)) >$< distanceWith2 d_EdgeOrien_UDSlice
  ]

phase1 :: Cube -> Move
phase1 =
    let moves = (,,) move18CornerOrien move18EdgeOrien move18UDSlice
        phase1Search = mkSearch move18Names moves phase1Proj phase1Dist
    in fromJust . search phase1Search . tag . phase1Convert

-- | > phase1Solved (phase1 c)
phase1Solved :: Cube -> Bool
phase1Solved = ((==) `on` phase1Convert) iden

--

phase2Proj
  =   rawProjection
  |*| rawProjection
  |.| rawProjection

phase2Convert = convertP phase2Proj

phase2Dist = maxDistance
  [ (\((,,) cp _ udsp) -> (cp, udsp)) >$< distanceWith2 d_CornerPermu_UDSlicePermu2
  , (\((,,) _ udep udsp) -> (udep, udsp)) >$< distanceWith2 d_UDEdgePermu2_UDSlicePermu2
  ]

phase2 :: Cube -> Move
phase2 =
    let moves = (,,) move10CornerPermu move10UDEdgePermu2 move10UDSlicePermu2
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
