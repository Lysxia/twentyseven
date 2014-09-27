{- | Some tables of numbers for fast look up. -}

module Tables where

import Coord
import Cubie
import Moves
import Misc ( Vector, composeVector )
import Symmetry

import Control.Monad

import qualified Data.Vector.Unboxed as U

symClassesFlipUDSlice :: Vector SymCoord
symClassesFlipUDSlice
  = symClasses
      coordFlipUDSlice
      (map conjugateFlipUDSlice sym16)



