{- | Pruning tables -}

{-# Language FlexibleContexts, ScopedTypeVariables, TypeFamilies,
    ViewPatterns #-}
module Rubik.Distances where

import Control.Monad

import Data.Foldable
import Data.Function
import Data.STRef
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.MBitVector as MBV

import Debug.NoTrace

type Coord = Int

-- | Given a graph (via a neighbors function), find the distances from a root
-- to all nodes.
distances :: forall a t v
  . ( Traversable t, Eq a, Num a, Integral a, Show a, G.Vector v a )
  => Int -> Coord -> (Coord -> t Coord) -> v a
distances n root neighbors = G.create (do
    traceM $ "DistanceT " ++ show n
    mv <- MG.replicate n (-1)
    mb <- MBV.replicate n False
    count <- newSTRef (0 :: Int)
    fill n root neighbors mv mb count 0
    return mv)

-- We use two algorithms to fill the vector @mv@ with the distance from the
-- root to every node. The first @fill@ is more efficient when @mv@ is
-- either small or mostly empty, and the second @fill'@ when @mv@ is large
-- and almost full.

-- Mark nodes at distance d from the root, by DFS. The values of the bit vector
-- marks visited nodes depending on the parity of d.
fill n root neighbors mv mb count d = do
  c <- readSTRef count
  fillFrom neighbors mv mb count d 0 root
  c' <- readSTRef count
  traceShowM (d, c')
  -- Unless there are no more reachable untouched cells.
  unless (c == c' || c' == n) $
    if c' < n `div` 10
    then fill n root neighbors mv mb count (d+1)
    else fill' n neighbors mv count d

-- This is in fact not quite a textbook DFS : we bound the recursion
-- depth by the distance of the current node to the root, in order not to
-- explode the stack. The search remains complete though.
fillFrom neighbors mv mb count d = fix $ \go dx x -> do
  dx' <- MG.read mv x
  if dx' == -1
  then do
    modifySTRef' count (+1)
    MG.unsafeWrite mv x d
    MBV.put mb x (fromIntegral $ d `mod` 2)
  else do
    test <- mb `MBV.test` x
    when (test == even d && dx == dx') $ do -- Unvisited
      mb `MBV.complement` x
      for_ (neighbors x) (go (dx+1))

-- For every node at distance d, mark all neighbors at
-- distance (d+1) from the root, by simply traversing the array.
fill' n neighbors mv count = fix $ \go d -> do
  c <- readSTRef count
  traceM $ "D " ++ show (d, c)
  for' 0 n $ \x -> do
    d' <- MG.unsafeRead mv x
    when (d' == d) $ do
      ys <- (filterM (\t -> fmap (-1 ==) (MG.read mv t)) . toList . neighbors) x
      for_ ys $ \y -> modifySTRef' count (+1) >> MG.unsafeWrite mv y (d+1)
  c' <- readSTRef count
  unless (c == c' || c' == n) $ go (d+1)

-- @forM_ [0 .. n-1]@ somehow runs out of memory
for' i n f | i == n = return ()
for' i n f = f i >> for' (i+1) n f
