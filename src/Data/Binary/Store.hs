{-# LANGUAGE RecordWildCards #-}
module Data.Binary.Store (
  Stored (..),
  Preload (..),
  store,
  retrieve,
  loadS,
  preloadFrom,
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Binary
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

import System.FilePath

data Stored a = Store
  { name :: String,
    value :: a }

data Preload a = PL
  { preload :: FilePath -> IO a,
    compact :: FilePath -> IO (),
    unwrapPL :: a }

instance Functor Preload where
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
  (PL p c u) >>= f = PL
      { preload = \path -> do
          a <- p path
          preload (f a) path,
        compact = \path -> c path >> compact (f u) path,
        unwrapPL = unwrapPL (f u) }

store :: Binary a => FilePath -> Stored a -> IO ()
store path (Store name a) = encodeFile (path </> name) a

retrieve :: Binary a => FilePath -> Stored a -> IO a
retrieve path (Store name _) = decodeFile (path </> name)

preloadFrom :: FilePath -> Preload a -> IO a
preloadFrom = flip preload

loadS :: Binary a => Stored a -> Preload a
loadS s = PL
  { preload = retrieve `flip` s,
    compact = store `flip` s,
    unwrapPL = value s }

