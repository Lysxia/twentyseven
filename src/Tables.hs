{- | Some tables of numbers for fast look up. -}

module Tables where

import Coord
import Cubie
import Moves
import Misc ( Vector )
import Symmetry

symClassesFlipUDSlice :: Vector SymCoord
symClassesFlipUDSlice
  = symClasses
      coordFlipUDSlice
      (map conjugateFlipUDSlice sym16)

move6Coord :: CubeAction a => Coordinate a -> [Vector Coord]
move6Coord coord = map (endoVector coord) endo6

move6FlipUDSlice :: [Vector Coord]
move6FlipUDSlice = move6Coord coordFlipUDSlice

move6CornerOrien :: [Vector Coord]
move6CornerOrien = move6Coord coordCornerOrien

