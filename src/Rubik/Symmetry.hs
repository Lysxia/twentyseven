{- |
 - Tables of symmetry classes
 -}
{-# Language ScopedTypeVariables, ViewPatterns #-}
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
type SymRepr a = RawCoord a

type SymClass' = Int
newtype SymClass symType a = SymClass { unSymClass :: SymClass' }

-- | An @Int@ representing a pair @(Repr, Sym)@.
--
-- If @x = symClass * symOrder + symCode@,
-- where @symClass :: Repr@ is the index of the symmetry class with
-- smallest representative @r@,
-- @symOrder@ is the size of the symmetry group,
-- @symCode :: Sym@ is the index of a symmetry @s@;
-- then @s^(-1) <> r <> s@ is the value represented by @x@.
type SymCoord' = Int

newtype Action s a = Action [a -> a]
newtype SymReprTable s a = SymReprTable (Vector RawCoord')
newtype SymMove s a = SymMove (Vector SymCoord')

-- | Compute the table of smallest representatives for all symmetry classes.
-- The @RawCoord'@ coordinate of that representative is a @Repr@.
-- The table is sorted in increasing order.
symClasses
  :: RawEncoding a {- ^ Coordinate encoding -}
  -> Action s a    {- ^ Symmetry group, including the identity,
                    -   represented by its action on @a@ -}
  -> SymReprTable s a {- ^ Smallest representative -}
symClasses c sym = SymReprTable . U.fromList . fmap unRawCoord $ symClasses' c sym

symClasses' :: forall a s. RawEncoding a -> Action s a -> [RawCoord a]
symClasses' c (Action sym)
  = foldFilter (H.empty :: H.MinHeap (RawCoord a)) (fmap RawCoord [0 .. range c - 1])
  where
    foldFilter _ [] = []
    foldFilter (H.view -> Nothing) (x : xs) = x : foldFilter (heapOf x) xs
    foldFilter (h@(H.view -> Just (y, ys))) (x : xs)
      | x < y = x : foldFilter (H.union h (heapOf x)) xs
      | otherwise = foldFilter ys xs
    heapOf :: RawCoord a -> H.MinHeap (RawCoord a)
    heapOf x
      = let dx = decode c x
            nub' = map head . group . sort
        in H.fromAscList . tail . nub' $ map (\z -> encode c . z $ dx) sym

-- |
symMoveTable
  :: RawEncoding a
  -> Action s a      {- ^ Symmetry group -}
  -> SymReprTable s a   {- ^ (Sorted) table of representatives -}
  -> (a -> a)        {- ^ Endofunction to encode -}
  -> SymMove s a
symMoveTable enc action@(Action syms) (SymReprTable reps) f = SymMove $ U.map move reps
  where
    n = length syms
    symRepr = symReprMin enc action
    move x = fromJust (iFind r reps) * n + s
      where
        (SymCode r, RawCoord s) = symRepr . f . decode enc . RawCoord $ x

-- | Find the representative as the one corresponding to the smallest coordinate
symReprMin :: RawEncoding a -> Action s a -> a -> (SymCode s, SymRepr a)
symReprMin c (Action syms) x = (SymCode . fromJust $ elemIndex r xSym, r)
  where
    xSym = [ encode c (s x) | s <- syms ]
    r = minimum xSym

