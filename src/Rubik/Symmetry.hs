{- |
 - Tables of symmetry classes
 -}
{-# Language GeneralizedNewtypeDeriving, ScopedTypeVariables, ViewPatterns #-}
module Rubik.Symmetry where

import Rubik.Cube
import Rubik.Misc

import Control.Applicative

import Data.Binary (Binary)
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Heap as H
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | Smallest representative of a symmetry class.
-- (An element of the symClasses table)
type SymRepr a = RawCoord a

type SymClass' = Int
-- | Symmetry class. (Index of the smallest representative in the symClasses table)
newtype SymClass symType a = SymClass { unSymClass :: SymClass' } deriving (Eq, Ord, Show)

type SymCoord sym a = (SymClass sym a, SymCode sym)

-- | An @Int@ representing a pair @(Repr, Sym)@.
--
-- If @x = symClass * symOrder + symCode@,
-- where @symClass :: SymClass@ is the index of the symmetry class with
-- smallest representative @r :: SymRepr@ (for an arbitrary order relation),
-- @symOrder@ is the size of the symmetry group,
-- @symCode :: Sym@ is the index of a symmetry @s@;
-- then @s^(-1) <> r <> s@ is the value represented by @x@.
type SymCoord' = Int
type SymOrder' = Int

newtype Action s a = Action [a -> a]
newtype SymReprTable s a = SymReprTable (Vector RawCoord')
newtype SymMove s a = SymMove (Vector SymCoord') deriving Binary

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
symMoveTable enc action@(Action syms) reps'@(SymReprTable reps) f
  = SymMove $ U.map move reps
  where
    n = length syms
    symRepr = symReprMin enc action reps'
    move x = flatIndex n c s
      where
        (SymClass c, SymCode s) = symRepr . f . decode enc . RawCoord $ x

symMove :: SymOrder' -> SymMove s a -> SymClass s a -> (SymClass s a, SymCode s)
symMove n (SymMove v) (SymClass x) = (SymClass y, SymCode i)
  where (y, i) = divMod n (v U.! x)

symMove' n v (x, j) = (y, i `composeSym` j)
  where (y, i) = symMove n v x

-- | Find the representative as the one corresponding to the smallest coordinate
symReprMin :: RawEncoding a -> Action s a -> SymReprTable s a
  -> a -> (SymClass s a, SymCode s)
symReprMin c (Action syms) (SymReprTable reps) x
  = (SymClass . fromJust $ iFind r reps, SymCode s)
  where
    xSym = [ encode c (s x) | s <- syms ]
    (s, RawCoord r) = minimumBy (comparing snd) $ zip [0 ..] xSym

