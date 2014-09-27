{- | Two phase algorithm to solve a Rubik's cube -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TwoPhase where

import Coord
import Cubie
import Distances
import IDA
import Moves
import Misc ( Vector, composeVector, Group (..) )

import Control.Applicative
import Control.Monad

import Data.Binary hiding ( encode, decode )
import Data.List
import Data.Vector.Binary
import qualified Data.Vector.Unboxed as U

import System.IO

-- | Minimum amount of information necessary to generate all tables for phase 1.
--
-- All values of type @Phase1Minimal@ are expected to be equal, although it is
-- costly to ensure that it is the case. The same requirement applies to
-- @Phase1@.
newtype Phase1Minimal = Phase1Minimal {
  move6Phase1 :: [[Vector Coord]]
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

-- | Using this representation generates tables from scratch.
phase1Minimal' :: Phase1Minimal
phase1Minimal' = Phase1Minimal
  [move6Coord coordFlipUDSlice, move6Coord coordCornerOrien]
  where 
    move6Coord :: CubeAction a => Coordinate a -> [Vector Coord]
    move6Coord coord = endoVector coord <$> endo6

-- | Generate all move and pruning tables.
phase1Expand :: Phase1Minimal -> Phase1
phase1Expand (Phase1Minimal vs) = Phase1 {
  move18Phase1 = move18Phase1',
  distPhase1 = zipWith tableToDistance
    (phase1Unwrap . phase1Encode $ iden)
    move18Phase1'
  }
  where
    move18Phase1' = (iterate3Vector =<<) <$> vs
    iterate3Vector = join ((take 3 .) . iterate . composeVector)
    tableToDistance root vs = distances (U.length (head vs)) root neighbors
      where neighbors x = map (U.! x) vs

phase1EncodeFile :: FilePath -> Phase1Minimal -> IO ()
phase1EncodeFile = encodeFile

phase1DecodeFile :: FilePath -> IO Phase1Minimal
phase1DecodeFile = decodeFile

-- | Phase 1 uses @FlipUDSlice@ and @CornerOrien@.
phase1Encode :: Cube -> Phase1Coord
phase1Encode c = Phase1Coord [
  encode coordFlipUDSlice (fromCube c),
  encode coordCornerOrien (cornerO c)
  ]

gsPhase1 :: Phase1 -> Cube -> GraphSearch (Int, String) Int Phase1Coord
gsPhase1 p c = GS {
  root = phase1Encode c,
  goal = (== phase1Encode iden),
  estm = maximum . zipWith (U.!) (distPhase1 p) . phase1Unwrap,
  succs = zipWith (flip Succ 1) (zip [0 ..] move18Names)
          . map Phase1Coord
          . transpose . zipWith (flip (map . flip (U.!))) (move18Phase1 p)
          . phase1Unwrap
  }

phase1 :: Phase1 -> Cube -> Maybe [(Int, String)]
phase1 p c = snd <$> search' (gsPhase1 p c)

