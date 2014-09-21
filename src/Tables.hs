{- | Some tables of numbers for fast look up. -}

module Tables where

import Coord
import Cubie
import Moves
import Symmetry

import qualified Data.Vector.Unboxed as U

symClassesFlipUDSlice :: U.Vector SymCoord
symClassesFlipUDSlice
  = symClasses
      coordFlipUDSlice
      (map conjugateFlipUDSlice sym16)

move18Coord :: CubeAction a => Coordinate a -> [Coord -> Coord]
move18Coord coord = map (endoLift coord) endo18

move18FlipUDSlice :: [Coord -> Coord]
move18FlipUDSlice = move18Coord coordFlipUDSlice

move18CornerOrien :: [Coord -> Coord]
move18CornerOrien = move18Coord coordCornerOrien

