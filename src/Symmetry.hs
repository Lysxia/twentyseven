{- |
 - Tables of symmetry classes
 -}
{-# Language ViewPatterns #-}
module Symmetry where

import Coord
import Cubie
import Misc

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
symClasses' c sym = foldFilter (H.empty :: H.MinHeap Coord) [0 .. cMax c]
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

