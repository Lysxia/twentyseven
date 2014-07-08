{- |
 - Tables of symmetry classes
 -}

module Symmetry where

import Coord
import Cubie
import Misc
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | Compute the table of smallest representatives for all symmetry classes
symClasses
  :: Coordinate a {- ^ Coordinate encoding -}
  -> [a -> a]     {- ^ Symmetry group -}
  -> Vector Coord {- ^ Smallest representative -}
symClasses c sym = U.fromList $ filter smallest [0 .. cMax c]
  where smallest x = all ((x <=) . encode c) $ map ($ decode c x) sym
