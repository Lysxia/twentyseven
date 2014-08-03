{- |
 - Tables of symmetry classes
 -}

module Symmetry where

import Coord
import Cubie
import Misc

import Data.List
import Data.Maybe
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

type SymCoord = Int

-- | Compute the table of smallest representatives for all symmetry classes.
-- The @Coord@ coordinate of that representative is a @SymCoord@.
-- The table is sorted in increasing order.
symClasses
  :: Coordinate a    {- ^ Coordinate encoding -}
  -> [a -> a]        {- ^ Symmetry group including the identity,
                      -   encoded as its action on @a@ -}
  -> Vector SymCoord {- ^ Smallest representative -}
symClasses c sym = U.fromList $ symClasses' c sym

symClasses'
  :: Coordinate a    {- ^ Coordinate encoding -}
  -> [a -> a]        {- ^ Symmetry group, including the identity -}
  -> [SymCoord] {- ^ Smallest representative -}
symClasses' c sym = foldFilter S.empty [0 .. cMax c]
  where
    foldFilter s (x : xs)
      | x `S.member` s = foldFilter s xs
      | otherwise
      = let dx = decode c x
        in x : foldFilter
             (S.union s . S.fromList $ map (\z -> encode c . z $ dx) sym)
             xs
    foldFilter _ [] = []

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

