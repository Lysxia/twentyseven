{- | Pruning tables -}

{-# Language ViewPatterns #-}
module Rubik.Distances where

import Control.Monad
import Control.Monad.ST

import Data.Int ( Int8 )
import Data.Queue as Q
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Debug.Trace

type Coord = Int

-- | Distances only go up to 20 for 3x3 Rubik's cubes.
type DInt = Int8

-- | Given a graph (via a neighbors function),
-- find the distances from a root to all nodes.
distances
  :: (Eq a, Num a, MU.Unbox a)
  => Int                -- ^ Number @n@ of vertices (labelled from @0@ to @n-1@)
  -> Coord              -- ^ Root
  -> (Coord -> [Coord]) -- ^ Neighbors (unit distance)
  -> U.Vector a
distances n root neighbors = U.create (do
    traceM "TEST"
    traceShowM n
    traceM "TEST"
    count <- newSTRef 0
    let wr a b c = do
          MU.write a b c
          modifySTRef count (+1)
          z <- readSTRef count
          when (z `mod` (n `div` 1000000) == 0) $
            traceShowM (z `div` (n `div` 1000000))
    mv <- MU.replicate n (-1)
    wr {- MU.write -} mv root 0
    breadthFirst wr mv (Q.singleton root)
    return mv)
  where
    breadthFirst _  _ (view -> Nothing) = return ()
    breadthFirst wr mv (view -> Just (x, q)) = do
      dx <- MU.read mv x
      ys <- filterM (fmap (-1 ==) . MU.read mv) $ neighbors x
      forM_ ys (\y -> wr {- MU.write -} mv y (dx+1))
      breadthFirst wr mv $ Q.append ys q
