{- | Two phase algorithm to solve a Rubik's cube -}

{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
module Rubik.Solver.TwoPhase where

import Rubik.Cube
import Rubik.IDA
import Rubik.Misc
import Rubik.Symmetry
import Rubik.Solver
import Rubik.Tables.Moves
import Rubik.Tables.Distances

import Control.Applicative
import Control.Monad

import Data.Binary.Store
import Data.Coerce
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

phase1Proj
  =   projCornerOrien
  |:| projEdgeOrien
  |.| projUDSlice

phase1Convert = convertP phase1Proj

phase1Dist = maxDistance
  [ (\(Tuple3 co _ uds) -> (co, uds)) >$< distanceWith2 d_CornerOrien_UDSlice rawUDSlice
  , (\(Tuple3 _ eo uds) -> (eo, uds)) >$< distanceWith2 d_EdgeOrien_UDSlice rawUDSlice
  ]

--phase1PL =
--  ( liftA3 (\(MoveTag a) (MoveTag b) (MoveTag c) -> MoveTag (Tuple3 a b c))
--      (loadS move18CornerOrien) (loadS move18EdgeOrien) (loadS move18UDSlice) )
--  ( liftA2 phase1Dist (loadS d_CornerOrien_UDSlice) (loadS d_EdgeOrien_UDSlice) )

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
  =   projCornerPermu
  |:| projUDEdgePermu2
  |.| projUDSlicePermu2

phase2Convert = convertP phase2Proj

phase2Dist =
  maxDistance
    [ (\(Tuple3 cp _ udsp) -> (cp, udsp)) >$< distanceWith2 d_CornerPermu_UDSlicePermu2 rawUDSlicePermu2
    , (\(Tuple3 _ udep udsp) -> (udep, udsp)) >$< distanceWith2 d_UDEdgePermu2_UDSlicePermu2 rawUDSlicePermu2
    ]

-- phase2PL = -- liftA2 (,)
-- --  ( liftA3 (\(MoveTag a) (MoveTag b) (MoveTag c) -> MoveTag (Tuple3 a b c))
-- --      (loadS move10CornerPermu) (loadS move10UDEdgePermu2) (loadS move10UDSlicePermu2) )
--   ( liftA2
--       -- (\(Distance a) (Distance b) -> a `seq` b `seq` Distance $ \(Tuple3 cp udep udsp) ->
--       --  max (a (cp, udsp))
--       --      (b (udep, udsp)))
--       --
--       -- (\(Distance a) (Distance b) -> Distance $ \t ->
--       --  max (a . (\(Tuple3 cp _ udsp) -> (cp, udsp)) $ t)
--       --      (b . (\(Tuple3 _ udep udsp) -> (udep, udsp)) $ t))
--       phase2Dist
--       (loadS d_CornerPermu_UDSlicePermu2) (loadS d_UDEdgePermu2_UDSlicePermu2) )

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
