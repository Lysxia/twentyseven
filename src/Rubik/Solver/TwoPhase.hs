{- | Two phase algorithm to solve a Rubik's cube -}

{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
module Rubik.Solver.TwoPhase where

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

phase1PL = liftA2 (,)
  ( liftA3 (\(MoveTag a) (MoveTag b) (MoveTag c) -> MoveTag (zipWith3 Tuple3 a b c))
      (loadS move18CornerOrien) (loadS move18EdgeOrien) (loadS move18UDSlice) )
  ( liftA2 phase1Dist dist_CornerOrien_UDSlice dist_EdgeOrien_UDSlice )

phase1 :: FilePath -> IO (Cube -> Move)
phase1 p = do
    (moves, dist) <- preloadFrom p phase1PL
    let proj = projCornerOrien
           |:| projEdgeOrien
           |.| projUDSlice
        phase1Search = mkSearch move18Names moves proj dist
        convert = (,) 6 . convertP proj
    return $ fromJust . search phase1Search . convert

{-# INLINE phase1Dist #-}
phase1Dist d_co_uds d_eo_uds = maxDistance
  [ (\(Tuple3 co _ uds) -> (co, uds)) >$< d_co_uds
  , (\(Tuple3 _ eo uds) -> (eo, uds)) >$< d_eo_uds
  ]

phase2PL = liftA2 (,)
  ( liftA3 (\(MoveTag a) (MoveTag b) (MoveTag c) -> MoveTag (zipWith3 Tuple3 a b c))
      (loadS move10CornerPermu) (loadS move10UDEdgePermu2) (loadS move10UDSlicePermu2) )
  ( liftA2
      (\d1 d2 -> Distance $ \(Tuple3 cp udep udsp) -> max (d1 U.! flatIndex (range rawUDSlicePermu2) (unRawCoord cp) (unRawCoord udsp)) (d2 U.! flatIndex (range rawUDSlicePermu2) (unRawCoord udep) (unRawCoord udsp)))
      (loadS d_CornerPermu_UDSlicePermu2)
      (loadS d_UDEdgePermu2_UDSlicePermu2))
  --( liftA2 phase2Dist dist_CornerPermu_UDSlicePermu2 dist_UDEdgePermu2_UDSlicePermu2 )

phase2 :: FilePath -> IO (Cube -> Move)
phase2 p = do
    (moves, dist) <- preloadFrom p phase2PL
    let proj = projCornerPermu
           |:| projUDEdgePermu2
           |.| projUDSlicePermu2
        phase2Search = mkSearch move10Names moves proj dist
        convert = (,) 6 . convertP proj
    return $ fromJust . search phase2Search . convert

{-# INLINE phase2Dist #-}
phase2Dist d_cp_udsp d_udep_udsp = maxDistance
  [ (\(Tuple3 cp _ udsp) -> (cp, udsp)) >$< d_cp_udsp
  , (\(Tuple3 _ udep udsp) -> (udep, udsp)) >$< d_udep_udsp
  ]

{-
-- | > phase1Solved (phase1 c)
phase1Solved :: Cube -> Bool
phase1Solved = ((==) `on` phase1Encode) iden

--

-- Uses branching reduction
--
-- Instead of a factor 10, we have factors
--
-- - 9 after R, B;
-- - 8 after L, F;
-- - 7 after D;
-- - 4 after U.

-- | > phase1Solved c ==> phase2Solved (phase2 c)
phase2Solved :: Cube -> Bool
phase2Solved = (== iden)

-}

-- | Solve a scrambled Rubik's cube.
--
-- Make sure the cube is actually solvable with 'Cubie.solvable',
-- before calling this function.
twoPhase :: FilePath -> IO (Cube -> Move)
twoPhase p = do
  p1 <- phase1 p
  p2 <- phase2 p
  return $ \c -> {-# SCC twophase #-}
    let s1 = p1 c
        c1 = c <> moveToCube s1
        s2 = p2 c1
    in reduceMove $ s1 ++ s2
