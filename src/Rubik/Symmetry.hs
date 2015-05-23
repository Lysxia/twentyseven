{- |
 - Tables of symmetry classes
 -}
{-# Language ViewPatterns #-}
module Rubik.Symmetry where

import Rubik.Cube
import Rubik.Misc

import Control.Applicative

import Data.List
import Data.Maybe
import qualified Data.Heap as H
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | Index of a symmetry class (its smallest representative)
-- in the symClasses table.
type Repr = Int
-- | Index of a symmetry in a group represented by @Action a@.
type Sym = Int
-- | An @Int@ representing a pair @(Repr, Sym)@.
--
-- If @x = symClass * symOrder + symCode@,
-- where @symClass :: Repr@ is the index of the symmetry class with
-- smallest representative @r@,
-- @symOrder@ is the size of the symmetry group,
-- @symCode :: Sym@ is the index of a symmetry @s@;
-- then @s^(-1) <> r <> s@ is the value represented by @x@.
type SymCoord = Int
type Action a = [a -> a]

-- | Compute the table of smallest representatives for all symmetry classes.
-- The @Coord@ coordinate of that representative is a @Repr@.
-- The table is sorted in increasing order.
symClasses
  :: Coordinate a    {- ^ Coordinate encoding -}
  -> Action a        {- ^ Symmetry group, including the identity,
                      -   encoded as its action on @a@ -}
  -> Vector Coord {- ^ Smallest representative -}
symClasses c sym = U.fromList $ symClasses' c sym

symClasses'
  :: Coordinate a {- ^ Coordinate encoding -}
  -> Action a     {- ^ Symmetry group, including the identity -}
  -> [Coord]   {- ^ Smallest representative -}
symClasses' c sym = foldFilter (H.empty :: H.MinHeap Coord) [0 .. range c - 1]
  where
    foldFilter _ [] = []
    foldFilter (H.view -> Nothing) (x : xs) = x : foldFilter (heapOf x) xs
    foldFilter (h@(H.view -> Just (y, ys))) (x : xs)
      | x < y = x : foldFilter (H.union h (heapOf x)) xs
      | otherwise = foldFilter ys xs
    heapOf :: Coord -> H.MinHeap Coord
    heapOf x
      = let dx = decode c x
            nub' = map head . group . sort
        in H.fromAscList . tail . nub' $ map (\z -> encode c . z $ dx) sym

-- |
symMoveTable
  :: Coordinate a
  -> Action a {- ^ Symmetry group -}
  -> Vector Coord {- ^ (Sorted) table of representatives -}
  -> (a -> a)        {- ^ Endofunction to encode -}
  -> Vector SymCoord
symMoveTable c syms symT f = U.map move symT
  where
    n = length syms
    symRepr = symReprMin c syms
    move x = fromJust $ (\sClass -> sClass * n + s) <$> iFind r symT
      where
        (r, s) = symRepr . f . decode c $ x

-- | Find the representative as the one corresponding to the smallest coordinate
symReprMin :: Coordinate a -> Action a -> a -> (Repr, Sym)
symReprMin c syms x = (r, fromJust $ elemIndex r xSym)
  where
    xSym = [ encode c (s x) | s <- syms ]
    r = minimum xSym

