{-# LANGUAGE RecordWildCards #-}
module Data.Binary.Store (
  Binary,
  Store (..),
  Preload (..),
  store,
  save,
  load,
  loadS,
  preloadFrom,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

import System.FilePath

data Store a = Store
  { pack :: a -> ByteString
  , unpack :: ByteString -> a
  , name :: String
  , value :: a }

{-# INLINE store #-}
store :: Binary a => String -> a -> Store a
store = Store B.encode B.decode

data Preload a = PL
  { preload :: FilePath -> IO a,
    compact :: FilePath -> IO (),
    unwrapPL :: a }

instance Functor Preload where
  {-# INLINE fmap #-}
  fmap f (PL p c u) = PL
    { preload = fmap (fmap f) p,
      compact = c,
      unwrapPL = f u }

instance Applicative Preload where
  pure x = PL (const $ pure x) (const $ return ()) x
  (PL pf cf uf) <*> (PL px cx ux) = PL
    { preload = \path -> pf path <*> px path,
      compact = \path -> cf path >> cx path,
      unwrapPL = uf ux }

instance Monad Preload where
  {-# INLINE (>>=) #-}
  (PL p c u) >>= f = PL
      { preload = \path -> do
          a <- p path
          preload (f a) path,
        compact = \path -> c path >> compact (f u) path,
        unwrapPL = unwrapPL (f u) }


{-# INLINE save #-}
save :: FilePath -> Store a -> IO ()
save path a = BS.writeFile (path </> name a) (pack a (value a))

{-# INLINE load #-}
load :: FilePath -> Store a -> IO a
load path a = unpack a <$> BS.readFile (path </> name a)

{-# INLINE preloadFrom #-}
preloadFrom :: MonadIO m => FilePath -> Preload a -> m a
preloadFrom = fmap liftIO . flip preload

{-# INLINE loadS #-}
loadS :: Binary a => Store a -> Preload a
loadS s = PL
  { preload = load `flip` s,
    compact = save `flip` s,
    unwrapPL = value s }
