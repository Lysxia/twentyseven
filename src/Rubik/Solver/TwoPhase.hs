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

phase1 :: Preload (Cube -> Move)
phase1 = do
    proj
      <-   proj18CornerOrien
      |:|. proj18EdgeOrien
      |.|. proj18UDSlice
    dist <- phase1Dist <$> dist_CornerOrien_UDSlice <*> dist_EdgeOrien_UDSlice
    let phase1Search = mkSearch move18Names proj dist
        convert = (,) 6 . convertP proj
    return $ fromJust . search phase1Search . convert

{-# INLINE phase1Dist #-}
phase1Dist d_co_uds d_eo_uds = maxDistance
  [ (\(Tuple3 co _ uds) -> (co, uds)) >$< d_co_uds
  , (\(Tuple3 _ eo uds) -> (eo, uds)) >$< d_eo_uds
  ]

phase2 :: Preload (Cube -> Move)
phase2 = do
    proj
      <-   {-# SCC phase_2 #-} proj10CornerPermu
      |:|. proj10UDEdgePermu2
      |.|. proj10UDSlicePermu2
    dist <- phase2Dist
      <$> dist_CornerPermu_UDSlicePermu2
      <*> dist_UDEdgePermu2_UDSlicePermu2
    let phase2Search = mkSearch move10Names proj dist
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
twoPhase :: Preload (Cube -> Move)
twoPhase = do
  p1 <- phase1
  p2 <- phase2
  return $ \c -> {-# SCC twophase #-}
    let s1 = p1 c
        c1 = c <> moveToCube s1
        s2 = p2 c1
    in reduceMove $ s1 ++ s2
