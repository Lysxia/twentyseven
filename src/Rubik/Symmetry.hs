{- |
 - Tables of symmetry classes
 -}
{-# Language GeneralizedNewtypeDeriving, ScopedTypeVariables, ViewPatterns #-}
module Rubik.Symmetry where

import Rubik.Cube
import Rubik.Misc

import Control.Monad

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Heap as H
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Pinned as P
import qualified Data.Vector.Primitive.Mutable as MP

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
newtype SymClassTable s a = SymClassTable { unSymClassTable :: P.Vector RawCoord' }
newtype SymReprTable s a = SymReprTable { unSymReprTable :: P.Vector Int }
newtype SymMove s a = SymMove (P.Vector SymCoord')

-- | Compute the table of smallest representatives for all symmetry classes.
-- The @RawCoord'@ coordinate of that representative is a @Repr@.
-- The table is sorted in increasing order.
symClasses
  :: RawEncodable a
  => Action s a    {- ^ Symmetry group, including the identity,
                    -   represented by its action on @a@ -}
  -> SymClassTable s a {- ^ Smallest representative -}
symClasses = SymClassTable . P.fromList . fmap unRawCoord . symClasses'

symClasses' :: forall a s. RawEncodable a => Action s a -> [RawCoord a]
symClasses' action@(Action sym)
  = foldFilter (H.empty :: H.MinHeap (RawCoord a))
      (fmap RawCoord [0 .. range action - 1])
  where
    foldFilter _ [] = []
    foldFilter (H.view -> Nothing) (x : xs) = x : foldFilter (heapOf x) xs
    foldFilter (h@(H.view -> Just (y, ys))) (x : xs)
      | x < y = x : foldFilter (H.union h (heapOf x)) xs
      | otherwise = foldFilter ys xs
    heapOf :: RawCoord a -> H.MinHeap (RawCoord a)
    heapOf x
      = let dx = decode x
            nub' = map head . group . sort
        in H.fromAscList . tail . nub' $ map (\z -> (encode . z) dx) sym

symClassTable
  :: Int
  -> SymReprTable s a
  -> SymClassTable s a
symClassTable nSym (SymReprTable s)
  = SymClassTable . P.ifilter (==) $ P.map (`div` nSym) s

symReprTable'
  :: Int -- ^ Number of elements @n@
  -> Int -- ^ Number of symmetries @nSym@
  -> (Int -> [Int]) -- ^ @f x@, symmetrical elements to @x@, including itself
  -> P.Vector Int
  -- ^ @v@, where @(y, i) = (v ! x) `divMod` nSym@ gives
  -- the representative @y@ of the symmetry class of @x@
  -- and the index of one symmetry mapping @x@ to @y@:
  --
  -- > f x !! i == y.
symReprTable' n nSym f
  = P.create $ do
      v <- MP.replicate n (-1)
      forM_ [0 .. n-1] $ \x -> do
        let ys = f x
        y <- MP.read v x
        when (y == -1) .
          forM_ (zip [0 ..] (f x)) $ \(i, x') ->
            MP.write v x' (flatIndex nSym x i)
      return v

-- |
symMoveTable
  :: RawEncodable a
  => Action s a      {- ^ Symmetry group -}
  -> SymClassTable s a   {- ^ (Sorted) table of representatives -}
  -> (a -> a)        {- ^ Endofunction to encode -}
  -> SymMove s a
symMoveTable action@(Action syms) classes f
  = SymMove (P.mapPinned move (unSymClassTable classes))
  where
    n = length syms
    move = flat . symCoord action classes . f . decode . RawCoord
    flat (SymClass c, SymCode s) = flatIndex n c s

symMoveTable'
  :: RawEncodable a
  => Int -- ^ Symmetry group order
  -> SymReprTable s a
  -> SymClassTable s a
  -> (a -> a)
  -> SymMove s a
symMoveTable' nSym reps classes f
  = SymMove (P.mapPinned move (unSymClassTable classes))
  where
    move = flat . symCoord' nSym reps classes . encode . f . decode . RawCoord
    flat (SymClass c, SymCode s) = flatIndex nSym c s

{-# INLINE symMove #-}
symMove :: SymOrder' -> SymMove s a -> SymClass s a -> SymCoord s a
symMove n (SymMove v) (SymClass x) = (SymClass y, SymCode i)
  where (y, i) = (v P.! x) `divMod` n

{-# INLINE symMove' #-}
symMove' n v (x, j) = (y, i `composeSym` j)
  where (y, i) = symMove n v x

reprToClass :: SymClassTable s a -> RawCoord a -> SymClass s a
reprToClass (SymClassTable cls) = SymClass . fromJust . flip iFind cls . unRawCoord

-- | Find the representative as the one corresponding to the smallest coordinate
symCoord :: RawEncodable a => Action s a -> SymClassTable s a
  -> a -> SymCoord s a
symCoord (Action syms) classes x
  = (reprToClass classes r, SymCode s)
  where
    xSym = [ encode (s x) | s <- syms ]
    (r, s) = minimumBy (comparing fst) (zip xSym [0 ..])

symCoord' :: Int -> SymReprTable s a -> SymClassTable s a -> RawCoord a -> SymCoord s a
symCoord' nSym (SymReprTable reps) (SymClassTable classes) (RawCoord x)
  = (SymClass r, SymCode i)
  where
    (y, i) = (reps P.! x) `divMod` nSym
    r = fromJust $ iFind r classes
