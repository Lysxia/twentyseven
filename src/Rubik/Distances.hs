{- | Pruning tables -}

{-# Language ScopedTypeVariables, ViewPatterns #-}
module Rubik.Distances where

import Control.Monad

import Data.Bits
import Data.Foldable
import Data.Int ( Int8 )
import Data.Primitive (Prim)
import Data.STRef
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Pinned as P
import qualified Data.Vector.Primitive.Mutable as MP

import Debug.Trace

type Coord = Int

-- | Distances only go up to 20 for 3x3 Rubik's cubes.
type DInt = Int8

-- | Given a graph (via a neighbors function), find the distances from a root
-- to all nodes.
distances :: forall a t
  . (Traversable t, Eq a, Num a, FiniteBits a, Integral a, Prim a, Show a)
  => Int -> Coord -> (Coord -> t Coord) -> P.Vector a
distances n root neighbors = P.create (do
    traceM $ "DistanceT " ++ show n
    mv <- P.newPinned n
    MP.set mv (-1)
    count <- newSTRef (0 :: Int)
    fill mv count 0
    return mv)
  where
    bsz = finiteBitSize (0 :: a) - 1

    -- We use two algorithms to fill the vector @mv@ with the distance from the
    -- root to every node. The first @fill@ is more efficient when @mv@ is
    -- either small or mostly empty, and the second @fill'@ when @mv@ is large
    -- and almost full.

    -- Mark nodes at distance d from the root, by DFS. The
    -- highest bit marks visited nodes depending on the parity of d.
    fill mv count d = do
      c <- readSTRef count
      fillFrom mv count (d .|. shiftL d bsz) 0 root
      c' <- readSTRef count
      traceShowM (d, c')
      if c == c' || c' == n -- No more reachable cells.
      then when (d `mod` 2 /= 0) $ for' 0 n $ \x ->
        MP.unsafeModify mv (`complementBit` bsz) x
      -- The first condition ensures that the "visited" flag is set to 0
      -- before switching, i.e., that the value in every cell is exactly its
      -- distance from the root.
      else if d `mod` 2 /= 0 || c' < n `div` 100
        then fill mv count (d+1)
        else fill' mv count d

    fillFrom mv count d' dx x = do
      dx' <- MP.read mv x
      if dx' == -1
      then do
        modifySTRef' count (+1)
        MP.unsafeWrite mv x d'
      -- This is in fact not quite a textbook DFS : we bound the recursion
      -- depth by the distance of the current node to the root, in order not to
      -- explode the stack. The search remains complete though.
      else if testBit (dx' `xor` d') bsz && clearBit dx' bsz == dx -- Unvisited
      then do
        MP.unsafeWrite mv x (dx' `complementBit` bsz)
        for_ (neighbors x) (fillFrom mv count d' (dx+1))
      else return ()

    -- For every node at distance d, mark all neighbors at
    -- distance (d+1) from the root, by simply traversing the array.
    fill' mv count d = do
      c <- readSTRef count
      traceM $ "D " ++ show (d, c)
      for' 0 n $ \x -> do
        d' <- MP.unsafeRead mv x
        when (d' == d) $ do
          ys <- (filterM (\t -> fmap (-1 ==) (MP.read mv t)) . toList . neighbors) x
          for_ ys $ \y -> modifySTRef' count (+1) >> MP.unsafeWrite mv y (d+1)
      c' <- readSTRef count
      unless (c == c' || c' == n) $ fill' mv count (d+1)

    -- @forM_ [0 .. n-1]@ somehow runs out of memory
    for' i n f | i == n = return ()
    for' i n f = f i >> for' (i+1) n f
