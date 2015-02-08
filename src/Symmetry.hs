{- |
 - Tables of symmetry classes
 -}
{-# Language ViewPatterns #-}
module Symmetry where

import Coord
import Cubie
import Misc

import Control.Applicative

import Data.List
import Data.Maybe
import qualified Data.Heap as H
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

type SymCoord = Int

-- | Compute the table of smallest representatives for all symmetry classes.
-- The @Coord@ coordinate of that representative is a @SymCoord@.
-- The table is sorted in increasing order.
symClasses
  :: Coordinate a    {- ^ Coordinate encoding -}
  -> [a -> a]        {- ^ Symmetry group, including the identity,
                      -   encoded as its action on @a@ -}
  -> Vector SymCoord {- ^ Smallest representative -}
symClasses c sym = U.fromList $ symClasses' c sym

symClasses'
  :: Coordinate a {- ^ Coordinate encoding -}
  -> [a -> a]     {- ^ Symmetry group, including the identity -}
  -> [SymCoord]   {- ^ Smallest representative -}
symClasses' c sym = foldFilter (H.empty :: H.MinHeap Coord) [0 .. range c - 1]
  where
    foldFilter _ [] = []
    foldFilter (H.view -> Nothing) (x : xs) = x : foldFilter (heapOf x) xs
    foldFilter (h@(H.view -> Just (y, ys))) (x : xs)
      | x < y = x : foldFilter (H.union h (heapOf x)) xs
      | otherwise = foldFilter ys xs
    heapOf x
      = let dx = decode c x
            nub' = map head . group . sort
        in H.fromAscList . tail . nub' $ map (\z -> encode c . z $ dx) sym

-- |
symMoveTable
  :: Coordinate a
  -> [a -> a] {- ^ Symmetry group -}
  -> Vector SymCoord {- ^ (Sorted) table of representatives -}
  -> (a -> a)        {- ^ Endofunction to encode -}
  -> Vector Int {- ^ The i-th cell contains `symClass * symOrder + symCode`
                     where symClass is the index of the symmetry class with
                     representative r,
                     symOrder is the size of the symmetry group,
                     symCode is the index of a symmetry s;
                     s^(-1) <> r <> s is the image of i -}
symMoveTable c syms symT f = U.map move symT
  where
    n = length syms
    symRepr = symReprMin c syms
    move x = fromJust $ (\sClass -> sClass * n + s) <$> iFind r symT
      where
        (r, s) = symRepr . f . decode c $ x

-- | Find the representative as the one corresponding to the smallest coordinate
symReprMin :: Coordinate a -> [a -> a] -> a -> (SymCoord, Int)
symReprMin c syms x = (r, fromJust $ elemIndex r xSym)
  where
    xSym = [ encode c (s x) | s <- syms ]
    r = minimum xSym

