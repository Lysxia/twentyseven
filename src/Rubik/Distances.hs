{- | Pruning tables -}

{-# Language FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeFamilies,
    ViewPatterns #-}
module Rubik.Distances where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Ref

import Data.Foldable
import Data.Function
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Generic.Mutable.Loops as MG
import qualified Data.MBitVector as MBV

type Coord = Int

{-# INLINE distances #-}
distances :: (Traversable t, Eq a, Integral a, Show a, G.Vector v a)
  => Int -> Coord -> (Coord -> t Coord) -> v a
distances n root neighbors = runST (distancesM MG.iForM_ n root neighbors)

-- | Given a graph (via a neighbors function), find the distances from a root
-- to all nodes.
{-# INLINE distancesM #-}
distancesM :: forall a m t r v
  . ( Traversable t, Eq a, Integral a, Show a
    , G.Vector v a, PrimMonad m, MonadRef r m )
  => MG.ILoop m (G.Mutable v) a -> Int -> Coord -> (Coord -> t Coord) -> m (v a)
distancesM forV n root neighbors = do
    mv <- MG.replicate n (-1)
    mb <- MBV.replicate n False
    count <- newRef (0 :: Int)
    fill forV n root neighbors mv mb count 0
    G.unsafeFreeze mv

-- We use two algorithms to fill the vector @mv@ with the distance from the
-- root to every node. The first @fill@ is more efficient when @mv@ is
-- either small or mostly empty, and the second @fill'@ when @mv@ is large
-- and almost full.

-- Mark nodes at distance d from the root, by DFS. The values of the bit vector
-- marks visited nodes depending on the parity of d.
{-# INLINE fill #-}
fill forV n root neighbors mv mb count = fix $ \go d -> do
  c <- readRef count
  fillFrom neighbors mv mb count d 0 root
  c' <- readRef count
  -- Unless there are no more reachable untouched cells.
  unless (c == c' || c' == n) $
    if c' < n `div` 10
    then go (d+1)
    else fill' forV n neighbors mv count d

-- This is in fact not quite a textbook DFS : we bound the recursion
-- depth by the distance of the current node to the root, in order not to
-- explode the stack. The search remains complete though.
{-# INLINE fillFrom #-}
fillFrom neighbors mv mb count d = fix $ \go dx x -> do
  dx' <- MG.read mv x
  if dx' == -1
  then do
    modifyRef' count (+1)
    MG.unsafeWrite mv x d
    MBV.put mb x (fromIntegral $ d `mod` 2)
  else do
    test <- mb `MBV.test` x
    when (dx == dx' && test == even d) $ do -- Unvisited
      mb `MBV.complement` x
      for_ (neighbors x) (go (dx+1))

-- For every node at distance d, mark all neighbors at
-- distance (d+1) from the root, by simply traversing the array.
{-# INLINE fill' #-}
fill' forV n neighbors mv count = fix $ \go d -> do
  c <- readRef count
  forV mv $ \x d' ->
    when (d' == d) $ do
      ys <- (filterM (\t -> fmap (-1 ==) (MG.read mv t)) . toList . neighbors) x
      for_ ys $ \y -> modifyRef' count (+1) >> MG.unsafeWrite mv y (d+1)
  c' <- readRef count
  unless (c == c' || c' == n) $ go (d+1)
