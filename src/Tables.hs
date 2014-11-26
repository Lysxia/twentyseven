{- | Some tables of numbers for fast look up. -}
module Tables (
  symClassesFlipUDSlice,
  )
  where

import Coord
import Cubie
import Moves
import Misc ( Vector, composeVector )
import Symmetry

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Binary
import qualified Data.Vector.Unboxed as U

import System.FilePath

data Stored a = Stored {
  store    :: IO (),
  retrieve :: IO a
  }

-- | The @Reader@ monad wraps the path to the root directory for
-- the program.
-- The preceding @FilePath@ argument is the name of the file to store
-- the first argument to, and later to read an equal value from.
mkStored :: Binary a => a -> FilePath -> Reader FilePath (Stored a)
mkStored a filename = ReaderT $ \path ->
  let filepath = path </> filename
  in return $ Stored {
      store = encodeFile filepath a,
      retrieve = decodeFile filepath
    }

symClassesFlipUDSlice :: Vector SymCoord
symClassesFlipUDSlice
  = symClasses
      coordFlipUDSlice
      (map conjugateFlipUDSlice sym16)

