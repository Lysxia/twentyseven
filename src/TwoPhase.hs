{- | Two phase algorithm to solve a Rubik's cube -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TwoPhase (
  twoPhase,
  -- * Phase 1
  Phase1Compressed,
  Phase1,
  Phase1Coord (..),
  phase1Compressed',
  phase1Expand,
  phase1,
  -- * Phase 2
  Phase2Compressed,
  Phase2,
  Phase2Coord (..),
  phase2Compressed',
  phase2Expand,
  phase2,
  -- * @binary@ utilities
  encodeFile,
  decodeFile
  )
  where

import Coord
import Cubie
import Distances
import IDA
import Moves
import Misc ( Vector, composeVector, Group (..) )
import Tables

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Binary ( encodeFile, decodeFile, Binary (..) )
import Data.List
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

-- | The move tables of @FlipUDSlice@ have length 1M, which is huge.
-- We turn the table for the 6 generating moves into a parameter,
-- so that we can precompute it and store it in a file for a faster
-- initialization.
--
-- All values of type @Phase1Compressed@ are expected to be equal,
-- although it is costly to ensure that it is the case.
-- The same requirement applies to the type @Phase1@.
--
-- Utilities to store the precomputed tables are provided via the Binary
-- instance.
newtype Phase1Compressed = Phase1Compressed {
  move6FlipUDSlice :: [Vector Coord] -- Table of the 6 generating moves
  }
  deriving (Eq, Binary)

-- | All move and pruning tables for phase 1.
data Phase1 = Phase1 {
  move18Phase1 :: [[Vector Coord]],
  distPhase1   :: [Vector Int]
  }

-- | Phase 1 coordinate representation, a /pair/ (length-2 list) representing:
-- UD slice edge positions and edge orientations; corner orientations.
newtype Phase1Coord = Phase1Coord { phase1Unwrap :: [Int] }
  deriving Eq

move6Coord :: CubeAction a => Coordinate a -> [Vector Coord]
move6Coord = moveTables move6

-- | Using this representation generates tables from scratch.
--
-- Use this to precompute tables and store them with @encodeFile@.
-- Read files with @decodeFile@.
phase1Compressed' :: Phase1Compressed
phase1Compressed' = Phase1Compressed {
  move6FlipUDSlice = move6Coord coordFlipUDSlice
  }

move6CornerOrien' :: [Vector Coord]
move6CornerOrien' = move6Coord coordCornerOrien

-- | Generate all move and pruning tables.
phase1Expand :: Phase1Compressed -> Phase1
phase1Expand (Phase1Compressed vs) =
  move18Phase1' `deepseq`
  distPhase1' `deepseq`
  Phase1 {
    move18Phase1 = move18Phase1',
    distPhase1 = distPhase1'
  }
  where
    move18Phase1' = (iterate3Vector =<<) <$> [vs, move6CornerOrien']
    distPhase1' = zipWith tableToDistance
      (phase1Unwrap . phase1Encode $ iden)
      move18Phase1'
    iterate3Vector = take 3 . join (iterate . composeVector)

-- | Phase 1 uses @FlipUDSlice@ and @CornerOrien@.
phase1Encode :: Cube -> Phase1Coord
phase1Encode = Phase1Coord . (<*>) [
  encode coordFlipUDSlice . fromCube, -- Cannot factor @fromCube@:
  encode coordCornerOrien . fromCube  -- different instances involved
  ] . pure

gsPhase1 :: Phase1 -> Cube -> GraphSearch (Int, String) Int Phase1Coord
gsPhase1 p c = GS {
  root = phase1Encode c,
  goal = (== phase1Encode iden),
  estm = maximum . zipWith (U.!) (distPhase1 p) . phase1Unwrap,
  succs = zipWith (flip Succ 1) (zip [0 ..] move18Names)
          . map Phase1Coord
          . transpose . zipWith ((. pure) . liftA2 (U.!)) (move18Phase1 p) 
          . phase1Unwrap
  }

-- | Phase 1: reduce to \<U, D, L2, F2, R2, B2\>.
phase1 :: Phase1 -> Cube -> Maybe [(Int, String)]
phase1 p c = snd <$> search' (gsPhase1 p c)

data Phase2Compressed = Phase2Compressed {
  move6P2UDEdgePermu :: [Vector Coord],
  move6P2CornerPermu :: [Vector Coord]
  }
  deriving Eq

instance Binary Phase2Compressed where
  put (Phase2Compressed ue cp) = put ue >> put cp
  get = liftA2 Phase2Compressed get get

data Phase2 = Phase2 {
  move10Phase2 :: [[Vector Coord]],
  distPhase2 :: [Vector Int]
  }

newtype Phase2Coord = Phase2Coord { phase2Unwrap :: [Int] }
  deriving Eq

move6P2Coord :: CubeAction a => Coordinate a -> [Vector Coord]
move6P2Coord = moveTables move6'

move6P2UDSlicePermu' :: [Vector Coord]
move6P2UDSlicePermu' = move6P2Coord coordUDSlicePermu

phase2Compressed' :: Phase2Compressed
phase2Compressed' = Phase2Compressed {
  move6P2UDEdgePermu = move6P2Coord coordUDEdgePermu,
  move6P2CornerPermu = move6P2Coord coordCornerPermu
  }

phase2Expand :: Phase2Compressed -> Phase2
phase2Expand (Phase2Compressed ue cp) =
  move10Phase2' `deepseq`
  distPhase2 `deepseq`
  Phase2 {
    move10Phase2 = move10Phase2',
    distPhase2 = distPhase2'
  }
  where
    move10Phase2' = mkMove10 <$> [move6P2UDSlicePermu', ue, cp]
    distPhase2' = zipWith tableToDistance
      (phase2Unwrap . phase2Encode $ iden)
      move10Phase2'
    mkMove10 (u : d : lfrb) = ([u, d] >>= iterate3Vector) ++ lfrb
    iterate3Vector = take 3 . join (iterate . composeVector)

phase2Encode :: Cube -> Phase2Coord
phase2Encode c = Phase2Coord . map ($ c) $ [
  encode coordUDSlicePermu . fromCube,
  encode coordUDEdgePermu  . fromCube,
  encode coordCornerPermu  . fromCube
  ]

gsPhase2 :: Phase2 -> Cube -> GraphSearch (Int, String) Int Phase2Coord
gsPhase2 p c = GS {
  root = phase2Encode c,
  goal = (== phase2Encode iden),
  estm = maximum . zipWith (U.!) (distPhase2 p) . phase2Unwrap,
  succs = zipWith (flip Succ 1)
            (([0, 1, 2] ++ [15, 16, 17] ++ [4, 7 ..]) `zip` move10Names)
          . map Phase2Coord
          . transpose . zipWith (flip (map . flip (U.!))) (move10Phase2 p)
          . phase2Unwrap
  }

phase2 :: Phase2 -> Cube -> Maybe [(Int, String)]
phase2 p c = snd <$> search' (gsPhase2 p c)

--

twoPhase
  :: (Cube -> Maybe [(Int, String)])
  -> (Cube -> Maybe [(Int, String)])
  -> (Cube -> Maybe [(Int, String)])
twoPhase ph1 ph2 c = do
  s1 <- ph1 c
  let c' = foldl' (?) c (((move18 !!) . fst) <$> s1)
  s2 <- ph2 c'
  return (s1 ++ s2)

--

tableToDistance :: Coord -> [Vector Coord] -> Vector Int
tableToDistance root vs = distances (U.length (head vs)) root neighbors
  where neighbors = liftA2 (U.!) vs . pure

