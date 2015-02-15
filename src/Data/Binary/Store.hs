module Data.Binary.Store (
  Stored (..),
  store,
  retrieve,
  ) where

import Rubik.Cube
import Rubik.Misc ( Vector, composeVector )
import Rubik.Symmetry

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Binary
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U

import System.FilePath

data Stored a = Store {
  name :: FilePath,
  value :: a
  }

store :: Binary a => FilePath -> Stored a -> IO ()
store path (Store name a) = encodeFile (path </> name) a

retrieve :: Binary a => FilePath -> Stored a -> IO a
retrieve path (Store name _) = decodeFile (path </> name)

