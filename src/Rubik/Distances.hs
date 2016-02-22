{- | Pruning tables -}

{-# Language ViewPatterns #-}
module Rubik.Distances where

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe

import Data.Word ( Word32 )
import Data.Int ( Int8 )
import Data.Queue as Q
import Data.STRef
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS

import Foreign.ForeignPtr

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
    count <- newSTRef 0
    let wr a b c = do
          MU.write a b c
          modifySTRef count (+1)
          z <- readSTRef count
          let (q, r) = z `divMod` (n `div` 100)
          when (r == 0) (traceShowM q)
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

distances'
  :: (Eq a, Num a, S.Storable a)
  => Int -> Coord -> (Coord -> [Coord]) -> S.Vector a
distances' n root neighbors = S.create (do
    mv <- alloc n
    MS.set mv (-1)
    MS.write mv root 0
    fill mv 0
    return mv)
  where
    alloc n = unsafeIOToST
      (flip MS.unsafeFromForeignPtr0 n <$> mallocForeignPtrArray n)
    -- At every step, mark all neighbors at distance (d+1) of the root.
    fill mv d = do
      count <- newSTRef 0
      forM' 0 n $ \x -> do
        d' <- MS.read mv x
        when (d' == d) $ do
          modifySTRef' count (+1)
          ys <- filterM (\t -> fmap (-1 ==) (MS.read mv t)) (neighbors x)
          forM_ ys $ \y -> MS.write mv y (d+1)
      c <- readSTRef count
      when (c > 0) $ fill mv (d+1)
    forM' i n f | i == n = return ()
    forM' i n f = f i >> forM' (i+1) n f
