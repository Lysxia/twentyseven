-- | Vectors with integer values in '[0 .. 15]', which hold in half a byte.
-- This module is generic in the underlying vector, and specialized to 'Pinned'
-- in 'Data.Vector.HalfByte.Pinned'.

{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
    MagicHash, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables,
    TypeFamilies #-}
module Data.Vector.HalfByte where

import Control.DeepSeq
import Control.Monad
import Data.Binary.Storable
import Data.Bits
import Data.Coerce
import Data.Foldable
import Data.Primitive (sizeOf)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Storable.Allocated as S

newtype Word4 = Word4 { unWord4 :: Word }
  deriving (Eq, Ord, Real, Integral)

instance Enum Word4 where
  succ (Word4 x)
    | x == 15 = error "succ: Word4 maxBound"
    | otherwise = Word4 (succ x)
  pred (Word4 x) = Word4 (pred x)
  toEnum = Word4 . toEnum
  fromEnum = fromEnum . unWord4

instance Num Word4 where
  Word4 x + Word4 y = Word4 ((x + y) .&. 15)
  Word4 x * Word4 y = Word4 ((x * y) .&. 15)
  Word4 x - Word4 y = Word4 ((x - y) .&. 15)
  abs = id
  signum 0 = 0
  signum _ = 1
  fromInteger = word4 . fromInteger

instance Show Word4 where
  showsPrec = coerce (showsPrec :: Int -> Word -> ShowS)

instance Read Word4 where
  readsPrec = coerce (readsPrec :: Int -> ReadS Word)

type Vector' = Vector S.Vector Word4
type MVector' s = MVector S.MVector s Word4

data Vector v a = Vector !Int !Int !(v Word)
  -- ^ Offset (0,'sizeOf (_::Word)'), length, underlying vector.

data MVector v s a = MVector !Int !Int !(v s Word)

instance NFData (v Word) => NFData (Vector v Word4) where
  rnf (Vector _ _ v) = rnf v

instance G.Vector v Word => Show (Vector v Word4) where
  showsPrec = G.showsPrec

word4 :: Word -> Word4
word4 = Word4 . (.&. word4Ones)

{-# INLINE wordSize #-}
{-# INLINE wordSize2 #-}
{-# INLINE word4Bits #-}
wordSize, wordSize2, word4Bits :: Int
wordSize = sizeOf (undefined :: Word)
-- | Number of 'Word4' in a 'Word'
wordSize2 = 2 * wordSize
word4Bits = 4
{-# INLINE word4Ones #-}
word4Ones :: Word
word4Ones = 15

{-# INLINE replWord #-}
replWord :: Word4 -> Word
replWord (Word4 x)
  = foldl' (\z b -> z .|. (x `shiftL` (word4Bits * b))) 0 [0 .. wordSize2 - 1]

instance MG.MVector v Word => MG.MVector (MVector v) Word4 where
  {-# INLINE basicLength #-}
  basicLength (MVector _ n _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVector ofs _ v)
    = MVector ofs' m (MG.basicUnsafeSlice vOfs (1 + (ofs'+m-1) `div` wordSize2) v)
    where
      (vOfs, ofs') = (ofs + j) `divMod` wordSize2
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector _ _ v) (MVector _ _ w) = MG.basicOverlaps v w
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = MVector 0 n <$> MG.basicUnsafeNew (1 + (n-1) `div` wordSize2)
  {-# INLINE basicInitialize #-}
  basicInitialize (MVector _ _ v) = MG.basicInitialize v
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n x = MVector 0 n <$>
    MG.basicUnsafeReplicate (1 + (n-1) `div` wordSize2) (replWord x)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector ofs _ v) i
    = word4 . (`shiftR` (word4Bits * b)) <$> MG.basicUnsafeRead v j
    where
      (j, b) = (ofs + i) `divMod` wordSize2
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector ofs _ v) i (Word4 x)
    = MG.basicUnsafeRead v j >>= \y -> do
        let y' = (y .&. mask) .|. (x `shiftL` (word4Bits * b))
        MG.basicUnsafeWrite v j y'
    where
      (j, b) = (ofs + i) `divMod` wordSize2
      mask = complement (word4Ones `shiftL` (word4Bits * b))
  {-# INLINE basicSet #-}
  basicSet v0@(MVector ofs n v) x0@(Word4 x)
    = do
      let (m', b') = (ofs + n) `divMod` wordSize2
      MG.basicUnsafeRead v 0 >>= \y -> do
        let y' = foldl' set y [ofs .. min (ofs+n) wordSize2 - 1]
        MG.basicUnsafeWrite v 0 y'
      when (m' > 1) $ do
        let v' = MG.basicUnsafeSlice 1 (m'-1) v
            z = replWord x0
        MG.basicSet v' z
      when (ofs+n > wordSize2) $ do
        MG.basicUnsafeRead v m' >>= \y -> do
          let y' = foldl' set y [0 .. b'-1]
          MG.basicUnsafeWrite v m' y'
    where
      set y b =
        let mask = complement (word4Ones `shiftL` (word4Bits * b))
        in (y .&. mask) .|. (x `shiftL` (word4Bits * b))

type instance G.Mutable (Vector v) = MVector (G.Mutable v)

instance G.Vector v Word => G.Vector (Vector v) Word4 where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector ofs n mv) = Vector ofs n <$> G.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector ofs n v) = MVector ofs n <$> G.basicUnsafeThaw v
  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (Vector ofs _ v)
    = Vector ofs' m (G.basicUnsafeSlice vOfs (1 + (ofs'+m-1) `div` wordSize2) v)
    where
      (vOfs, ofs') = (ofs + j) `divMod` wordSize2
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector ofs _ v) i
    = word4 . (`shiftR` (word4Bits * b)) <$> G.basicUnsafeIndexM v j
    where
      (j, b) = (ofs + i) `divMod` wordSize2

instance Binary (v Word) => Binary (Vector v Word4) where
  put h (Vector ofs n v) = put h ofs >> put h n >> put h v
  get h = get h >>= \ofs -> get h >>= \n -> Vector ofs n <$> get h
