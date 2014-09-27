{- | Some tables of numbers for fast look up. -}

module Tables (
  symClassesFlipUDSlice,
  moveTables
  )
  where

import Coord
import Cubie
import Moves
import Misc ( Vector, composeVector )
import Symmetry

import Control.Applicative
import Control.Monad

import qualified Data.Vector.Unboxed as U

symClassesFlipUDSlice :: Vector SymCoord
symClassesFlipUDSlice
  = symClasses
      coordFlipUDSlice
      (map conjugateFlipUDSlice sym16)

moveTables :: CubeAction a => [Cube] -> Coordinate a -> [Vector Coord]
moveTables moves coord = (endoVector coord . flip cubeAction) <$> moves

