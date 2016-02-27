-- | An alternative to 'Data.Vector.Storable' where the underlying byte arrays
-- are pinned.

{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MagicHash,
    MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Data.Vector.Storable.Allocated where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Binary.Storable
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import Foreign
import System.IO

newtype Vector a = Vector { unV :: S.Vector a }
  deriving (Eq, Ord, Read, Show, NFData)

newtype MVector s a = MVector { unMV :: MS.MVector s a }
  deriving (NFData)

instance Storable a => MG.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength = MG.basicLength . unMV
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m = MVector . MG.basicUnsafeSlice j m . unMV
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector v) (MVector w) = MG.basicOverlaps v w
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew = fmap MVector . callocVector
  {-# INLINE basicInitialize #-}
  basicInitialize = MG.basicInitialize . unMV
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead = MG.basicUnsafeRead . unMV
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite = MG.basicUnsafeWrite . unMV
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector v) (MVector w) = MG.basicUnsafeCopy v w
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector v) (MVector w) = MG.basicUnsafeMove v w
  {-# INLINE basicSet #-}
  basicSet = MG.basicSet . unMV

type instance G.Mutable Vector = MVector

instance Storable a => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze = fmap Vector . G.basicUnsafeFreeze . unMV
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = fmap MVector . G.basicUnsafeThaw . unV
  {-# INLINE basicLength #-}
  basicLength = G.basicLength . unV
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n = Vector . G.basicUnsafeSlice j n . unV
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM = G.basicUnsafeIndexM . unV
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector mv) (Vector v) = G.basicUnsafeCopy mv v
  {-# INLINE elemseq #-}
  elemseq = G.elemseq . unV

{-# INLINE callocVector #-}
callocVector :: forall a m. (PrimMonad m, Storable a)
  => Int -> m (S.MVector (PrimState m) a)
callocVector n = unsafePrimToPrim $
    S.MVector n <$> (newForeignPtr finalizerFree =<< callocArray n)

-- * Mutable interface

{-# INLINE replicate #-}
replicate :: (PrimMonad m, Storable a) => Int -> a -> m (MVector (PrimState m) a)
replicate = MG.replicate

{-# INLINE read #-}
read :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
read = MG.read

{-# INLINE write #-}
write :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m ()
write = MG.write

{-# INLINE modify #-}
modify :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify = MG.modify

{-# INLINE unsafeRead #-}
unsafeRead :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
unsafeRead = MG.unsafeRead

{-# INLINE unsafeWrite #-}
unsafeWrite
  :: (PrimMonad m, Storable a) =>  MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite = MG.unsafeWrite

{-# INLINE unsafeModify #-}
unsafeModify
  :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify = MG.unsafeModify

-- * Immutable interface

{-# INLINE (!) #-}
(!) :: Storable a => Vector a -> Int -> a
(!) = (G.!)

{-# INLINE length #-}
length :: Storable a => Vector a -> Int
length = G.length

{-# INLINE generate #-}
generate :: Storable a => Int -> (Int -> a) -> Vector a
generate = G.generate

{-# INLINE create #-}
create :: Storable a => (forall s. ST s (MVector s a)) -> Vector a
create = G.create

{-# INLINE map #-}
map :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
map = G.map

{-# INLINE ifilter #-}
ifilter :: Storable a => (Int -> a -> Bool) -> Vector a -> Vector a
ifilter = G.ifilter

{-# INLINE fromList #-}
fromList :: Storable a => [a] -> Vector a
fromList = G.fromList

-- * IO

getMVector :: forall a. Storable a => Handle -> MVector RealWorld a -> IO ()
getMVector h (MVector (S.MVector n ptr))
  = withForeignPtr ptr $ \ptr -> hGetBuf h ptr n' >>= \m ->
      when (m /= n') $ fail "Not enough bytes."
  where
    n' = n * sizeOf (undefined :: a)

instance Storable a => Binary (Vector a) where
  put h (Vector v)
    = S.unsafeWith v $ \ptr -> put h n >> hPutBuf h ptr (n * size)
    where
      n = S.length v
      size = sizeOf (undefined :: a)

  get h = get h >>= \n -> MG.new n >>= getMVector h >>. G.unsafeFreeze
    where
      (>>.) = liftM2 (>>)
