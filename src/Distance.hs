{- |
 - Distance tables
 -}

{-# Language ViewPatterns #-}
module Distance where

import Coord

import Control.Arrow
import Control.Monad hiding (forM_)
import Control.Monad.ST.Safe

import Data.Dequeue as Dequeue (Dequeue (..), BankersDequeue)
import Data.Foldable
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

type Q = BankersDequeue
type Dist = Int

distanceFrom
  :: [Coord]          -- ^ Initial set
  -> Coord            -- ^ Bound on coordinates
  -> [Coord -> Coord] -- ^ Transitions (unit length)
  -> (Coord -> Dist)  -- ^ Distance function
distanceFrom zero cMax moves = (dist U.!)
  where
    dist = U.create (do
      v <- MU.new cMax
      toVisit <- MU.new cMax
      MU.set toVisit True
      let mark xs d = forM_ xs (\x -> MU.write v x d >> MU.write toVisit x False)
          notVisited = MU.read toVisit
      mark zero 0
      let q = Dequeue.fromList $ map (\x -> (x, 0)) zero :: Q (Coord, Dist)
      fill mark notVisited q
      return v)
    fill mark notVisited (popFront -> (Nothing, _)) = return ()
    fill mark notVisited (popFront -> (Just (x, d), q)) = do
      xs <- filterM notVisited $ map ($ x) moves
      mark xs (d + 1)
      let q' = foldl' pushBack q $ map (\x -> (x, d + 1)) xs
      fill mark notVisited q'

