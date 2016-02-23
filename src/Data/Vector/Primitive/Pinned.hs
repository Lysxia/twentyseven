{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
module Data.Vector.Primitive.Pinned where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as MP
import qualified Data.Vector.Fusion.Stream.Monadic as FS
import Data.Primitive
import GHC.Ptr -- What could go wrong
import System.IO

-- * Pinned interface

newPinned :: forall a m. (PrimMonad m, Prim a)
  => Int -> m (P.MVector (PrimState m) a)
newPinned n = P.MVector 0 n <$> newAlignedPinnedByteArray (n * size) align
  where
    size = sizeOf (undefined :: a)
    align = alignment (undefined :: a)

generatePinned :: Prim a => Int -> (Int -> a) -> P.Vector a
generatePinned n f = P.create $ do
  mv <- newPinned n
  forM_ [0 .. n-1] $ \i -> MP.write mv i (f i)
  return mv

-- * IO

getMVector :: forall a. Prim a => Handle -> P.MVector RealWorld a -> IO ()
getMVector h mv@(P.MVector _ n _)
  = hGetBuf h (mutableByteArrayPtr mv) n' >>= \m ->
      when (m /= n') $ fail "Not enough bytes."
  where
    size = sizeOf (undefined :: a)
    n' = n * size

getVector :: forall a. Prim a => Handle -> Int -> IO (P.Vector a)
getVector h n = newPinned n >>= getMVector h >>. P.unsafeFreeze
  where
    (>>.) = liftM2 (>>)

putVector :: forall a. Prim a => Handle -> P.Vector a -> IO ()
putVector h v@(P.Vector _ n _)
  = hPutBuf h (byteArrayPtr v) (n * size)
  where
    size = sizeOf (undefined :: a)

writeVectorFile :: Prim a => String -> P.Vector a -> IO ()
writeVectorFile file v = withBinaryFile file WriteMode $ \h -> putVector h v

readVectorFile :: Prim a => String -> Int -> IO (P.Vector a)
readVectorFile file n = withBinaryFile file ReadMode $ \h -> getVector h n

writeVectorListFile :: Prim a => String -> [P.Vector a] -> IO ()
writeVectorListFile file vs
  = withBinaryFile file WriteMode $ \h -> forM_ vs (putVector h)

readVectorListFile :: Prim a => String -> Int -> Int -> IO [P.Vector a]
readVectorListFile file k n
  = withBinaryFile file ReadMode $ \h -> replicateM k (getVector h n)

-- * Low level functions

mutableByteArrayPtr :: forall a s. Prim a => P.MVector s a -> Ptr a
mutableByteArrayPtr (P.MVector i _ ba)
  = addrToPtr (mutableByteArrayContents ba `plusAddr` i * size)
  where
    size = sizeOf (undefined :: a)

byteArrayPtr :: forall a. Prim a => P.Vector a -> Ptr a
byteArrayPtr (P.Vector i _ ba)
  = addrToPtr (byteArrayContents ba)
  where
    size = sizeOf (undefined :: a)

addrToPtr :: Addr -> Ptr a
addrToPtr (Addr addr#) = Ptr addr#
