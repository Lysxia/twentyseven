{- | Pruning tables -}

{-# Language ViewPatterns #-}
module Distances where

import Queue as Q

import Control.Monad
import Control.Monad.ST

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

type Coord = Int

-- | Given a graph (via a neighbors function),
-- find the distances from a root to all nodes.
distances
  :: Int                -- ^ Number @n@ of vertices (labelled from @0@ to @n-1@)
  -> Coord              -- ^ Root
  -> (Coord -> [Coord]) -- ^ Neighbors (unit distance)
  -> U.Vector Int
distances n root neighbors = U.create (do
    mv <- MU.replicate n (-1)
    MU.write mv root 0
    breadthFirst mv (Q.singleton root)
    return mv)
  where
    breadthFirst  _ (view -> Nothing) = return ()
    breadthFirst mv (view -> Just (x, q)) = do
      dx <- MU.read mv x
      ys <- filterM (fmap (-1 ==) . MU.read mv) $ neighbors x
      forM_ ys (\y -> MU.write mv y (dx+1))
      breadthFirst mv $ Q.append ys q

