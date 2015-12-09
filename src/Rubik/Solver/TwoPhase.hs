{- | Two phase algorithm to solve a Rubik's cube -}

{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
module Rubik.Solver.TwoPhase (
  twoPhase,

  -- * Phase 1
  Phase1Coord,
  phase1,
  phase1Solved,
  phase1DistTables,
  Phase1Tables (..),

  -- * Phase 2
  Phase2Coord,
  Phase2Distances,
  phase2,
  phase2Solved,
  phase2DistTables,
  Phase2Tables (..),
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
data Phase1Tables = Phase1Tables
  { m_CornerOrien :: [Vector Coord],
    m_EdgeOrien :: [Vector Coord],
    m_UDSlice :: [Vector Coord],
    d_CornerOrien_UDSlice :: Vector DInt,
    d_EdgeOrien_UDSlice :: Vector DInt }

data Phase2Tables = Phase2Tables
  { m_CornerPermu :: [Vector Coord],
    m_UDEdgePermu2 :: [Vector Coord],
    m_UDSlicePermu2 :: [Vector Coord],
    d_CornerPermu_UDSlicePermu2 :: Vector DInt,
    d_EdgePermu2 :: Vector DInt }

type Phase2Distances = [Vector DInt]

phase1 :: Preload (Cube -> Move)
phase1 = do
    t <- Phase1Tables 
           (movesCI move18CornerOrien)
           (movesCI move18EdgeOrien)
           (movesCI move18UDSlice)
           <$> loadS dist_CornerOrien_UDSlice
           <*> loadS dist_EdgeOrien_UDSlice
    return $ extract . search (phase1SearchWith t) . phase1Encode

phase2 :: Preload (Cube -> Move)
phase2 = do
    t <- Phase2Tables
           (movesCI move10CornerPermu)
           (movesCI move10UDEdgePermu2)
           (movesCI move10UDSlicePermu2)
           <$> loadS dist_CornerPermu_UDSlicePermu2
           <*> loadS dist_EdgePermu2
    let s =
          -- phase2Search' t -- 99s / 1000 cubes
          -- phase2SearchWith t -- 107s / 1000 cubes
          phase2SearchWith' t
    return $ extract . search s . phase2Encode

phase1SearchWith :: Phase1Tables -> Search [] DInt ElemMove (Tag Phase1Coord)
phase1SearchWith (Phase1Tables {..})
  = $(searchWithQ
      'move18Names
      'phase1Convert
      ['m_CornerOrien, 'm_EdgeOrien, 'm_UDSlice]
      [ TwoP 'rUDS 'm_CornerOrien 'm_UDSlice 'd_CornerOrien_UDSlice,
        TwoP 'rUDS 'm_EdgeOrien 'm_UDSlice 'd_EdgeOrien_UDSlice ])
  where
    rUDS = range coordUDSlice

phase2SearchWith :: Phase2Tables -> Search [] DInt ElemMove (Tag Phase2Coord)
phase2SearchWith (Phase2Tables {..})
  = $(searchWithQ
      'move10Names
      'phase2Convert
      ['m_CornerPermu, 'm_UDEdgePermu2, 'm_UDSlicePermu2]
      [ TwoP 'rUDSP 'm_CornerPermu 'm_UDSlicePermu2 'd_CornerPermu_UDSlicePermu2,
        TwoP 'rUDSP 'm_UDEdgePermu2 'm_UDSlicePermu2 'd_EdgePermu2 ])
  where
    rUDSP = range coordUDSlicePermu2

phase2SearchWith' :: Phase2Tables -> Search _ DInt ElemMove (Tag Phase2Coord)
phase2SearchWith' Phase2Tables{..} = mkSearch move10Names s d
  where
    cips ci = PreSearch (encodeCI ci) (encodeCI ci iden) `flip` (U.!)
    s = cips move10CornerPermu m_CornerPermu
      |:| cips move10UDEdgePermu2 m_UDEdgePermu2
      |.| cips move10UDSlicePermu2 m_UDSlicePermu2
    d = PreDistance $ \(Tuple3 cp ep sp) -> max
      (d_CornerPermu_UDSlicePermu2 U.! (rUDSP * cp + sp))
      (d_EdgePermu2 U.! (ep * rUDSP + sp))
    rUDSP = range coordUDSlicePermu2

phase2SearchWithD :: Phase2Distances -> Search V.Vector DInt ElemMove (Tag Phase2Coord)
phase2SearchWithD = searchWith move10Names phase2CI transpose3 . flip zip phase2DistIndices

phase1CI = Tuple3 move18CornerOrien move18EdgeOrien move18UDSlice

phase2CI = Tuple3 move10CornerPermu move10UDEdgePermu2 move10UDSlicePermu2

phase1Convert :: Cube -> Phase1Coord
phase1Convert = $(convertQ ['coordCornerOrien, 'coordEdgeOrien, 'coordUDSlice])
phase1Encode :: Cube -> Tag Phase1Coord
phase1Encode = (,) 6 . phase1Convert

phase2Convert = $(convertQ ['coordCornerPermu, 'coordUDEdgePermu2, 'coordUDSlicePermu2])
phase2Encode = (,) 6 . phase2Convert

phase1DistTables  = [ dist_CornerOrien_UDSlice, dist_EdgeOrien_UDSlice ]
phase1DistIndices = [ Two rUDS (co, uds),       Two rUDS (eo, uds) ]
  where
    rUDS = range coordUDSlice
    [co, eo, uds] = [0 .. 2]

phase2DistTables  = [ dist_CornerPermu_UDSlicePermu2, dist_EdgePermu2 ]
phase2DistIndices = [ Two rUDSP (cp, udsp),           Two rUDSP (ude, udsp) ]
  where
    rUDSP = range coordUDSlicePermu2
    [cp, ude, udsp] = [0 .. 2]

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
phase2Search' :: Phase2Tables -> Search V.Vector DInt ElemMove (Tag Phase2Coord)
phase2Search' (Phase2Tables {..}) = Search {
    goal = goalSearch phase2CI,
    estm = \(_, Tuple3 cp ep uds)
      -> max (d_EdgePermu2 U.! flatIndex rUDSP ep uds)
             (d_CornerPermu_UDSlicePermu2 U.! flatIndex rUDSP cp uds),
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
        (transpose3 $ Tuple3 m_CornerPermu m_UDEdgePermu2 m_UDSlicePermu2)

-- | > phase1Solved c ==> phase2Solved (phase2 c)
phase2Solved :: Cube -> Bool
phase2Solved = (== iden)

--

-- | Solve a scrambled Rubik's cube.
--
-- Make sure the cube is actually solvable with 'Cubie.solvable',
-- before calling this function.
twoPhase :: Preload (Cube -> Move)
twoPhase = do
  p1 <- phase1
  p2 <- phase2
  return $ \c ->
    let s1 = p1 c
        c1 = c <> moveToCube s1
        s2 = p2 c1
    in reduceMove $ s1 ++ s2

