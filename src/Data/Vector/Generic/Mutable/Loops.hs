-- | Traversing mutable vectors.
module Data.Vector.Generic.Mutable.Loops where

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable as MG

type Loop m v a = v (PrimState m) a -> (a -> m ()) -> m ()
type ILoop m v a = v (PrimState m) a -> (Int -> a -> m ()) -> m ()

{-# INLINE iForM_ #-}
iForM_ :: (MG.MVector v a, PrimMonad m) => ILoop m v a
iForM_ v f = for' 0 (MG.length v) $ \i -> MG.unsafeRead v i >>= f i

{-# INLINE forM_ #-}
forM_ :: (MG.MVector v a, PrimMonad m) => Loop m v a
forM_ v = iForM_ v . const

-- @forM_ [0 .. n-1]@ somehow runs out of memory
for' i n f | i == n = return ()
for' i n f = f i >> for' (i+1) n f
