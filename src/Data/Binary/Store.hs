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
    unwrapPL :: a }

instance Functor Preload where
  fmap f (PL p u) = PL
    { preload = fmap (fmap f) p,
      unwrapPL = f u }

instance Applicative Preload where
  pure x = PL (const (pure x)) x
  (PL pf uf) <*> (PL px ux) = PL
    { preload = \path -> pf path <*> px path,
      unwrapPL = uf ux }

instance Monad Preload where
  (PL p u) >>= f = PL
      { preload = \path -> do
          a <- p path
          preload (f a) path,
        unwrapPL = unwrapPL (f u) }
  return = pure

store :: Binary a => FilePath -> Stored a -> IO ()
store path (Store name a) = encodeFile (path </> name) a

retrieve :: Binary a => FilePath -> Stored a -> IO a
retrieve path (Store name _) = decodeFile (path </> name)

preloadFrom = flip preload

loadS :: Binary a => Stored a -> Preload a
loadS s = PL
  { preload = retrieve `flip` s,
    unwrapPL = value s }

