-- | A binary-(the library)-like module which communicates directly through
-- handles rather than using bytestring.

{-# LANGUAGE ScopedTypeVariables #-}
module Data.Binary.Storable where

import Control.Monad
import qualified Data.Vector as V
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import System.IO

type Put a = Handle -> a -> IO ()
type Get a = Handle -> IO a

class Binary a where
  put :: Put a
  get :: Get a

storablePut :: Storable a => Put a
storablePut h a = with a (\ptr -> hPutBuf h ptr (sizeOf a))

storableGet :: forall a. Storable a => Get a
storableGet h = alloca (\ptr -> hGetBuf h ptr (sizeOf (undefined :: a)) >> peek ptr)

instance Binary Int where
  put = storablePut
  get = storableGet

instance Binary a => Binary [a] where
  put h as = put h (length as) >> forM_ as (put h)
  get h = get h >>= \n -> replicateM n (get h)

instance Binary a => Binary (V.Vector a) where
  put h = put h . V.toList
  get h = V.fromList <$> get h

encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile file a = withBinaryFile file WriteMode $ \h -> put h a

decodeFile :: Binary a => FilePath -> IO a
decodeFile file = withBinaryFile file ReadMode $ \h -> get h
