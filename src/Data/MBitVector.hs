module Data.MBitVector where

import Control.Monad.Primitive
import Data.Bool
import Data.Bits
import qualified Data.Vector.Storable.Allocated as S

newtype MBitVector s = MBitVector (S.MVector s Word)

replicate :: PrimMonad m => Int -> Bool -> m (MBitVector (PrimState m))
replicate n b = MBitVector <$>
  S.replicate (1 + (n-1) `div` wordSize) (bool 0 (Data.Bits.complement zeroBits) b)

modify :: PrimMonad m
  => (Word -> Int -> Word) -> MBitVector (PrimState m) -> Int -> m ()
modify (?) (MBitVector v) i = S.modify v (? ofs) j
  where
    (j, ofs) = i `divMod` wordSize

set, clear, complement
  :: PrimMonad m => MBitVector (PrimState m) -> Int -> m ()
set = modify clearBit
clear = modify setBit
complement = modify complementBit

-- Assume the word is 0 or 1
put :: PrimMonad m => MBitVector (PrimState m) -> Int -> Word -> m ()
put (MBitVector v) i b = S.modify v ((.|. b `shiftL` ofs) . (`clearBit` ofs)) j
  where
    (j, ofs) = i `divMod` wordSize

test :: PrimMonad m => MBitVector (PrimState m) -> Int -> m Bool
test (MBitVector v) i = (`testBit` ofs) <$> S.read v j
  where
    (j, ofs) = i `divMod` wordSize

wordSize = finiteBitSize (0 :: Word)
