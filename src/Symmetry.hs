{- |
 - Tables of symmetry classes
 -}

module Symmetry where

import Coord
import Cubie
import Misc

import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

type SymCoord = Int

-- | Compute the table of smallest representatives for all symmetry classes.
-- The table is sorted in increasing order.
symClasses
  :: Coordinate a    {- ^ Coordinate encoding -}
  -> [a -> a]        {- ^ Symmetry group, including the identity -}
  -> Vector SymCoord {- ^ Smallest representative -}
symClasses c sym = U.fromList $ filter smallest [0 .. cMax c]
  where smallest x = all ((x <=) . encode c) $ map ($ decode c x) sym

-- |
symMoveTable
  :: Coordinate a
  -> [a -> a]            {- ^ Symmetry group, including the identity -}
  -> Vector SymCoord     {- ^ (Sorted) table of representatives -}
  -> (a -> a)            {- ^ Endofunction to encode -}
  -> Vector (Int, SymCoord)
symMoveTable c sym symT f = U.map move symT
  where
    move x = fromJust $ do
      i <- elemIndex min fxs
      s <- iFind min symT
      return (i, s)
      where
        fxs = map (encode c . ($ f (decode c x))) sym
        min = minimum fxs

