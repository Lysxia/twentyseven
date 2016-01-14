{- |
 - Tables of symmetry classes
 -}
{-# Language GeneralizedNewtypeDeriving, ScopedTypeVariables, ViewPatterns #-}
module Rubik.Symmetry where

import Rubik.Cube
import Rubik.Misc

import Control.Applicative
import Control.Monad

import Data.Binary (Binary)
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Heap as H
import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Debug.Trace

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
newtype SymClassTable s a = SymClassTable { unSymClassTable :: Vector RawCoord' }
  deriving Binary
newtype SymReprTable s a = SymReprTable { unSymReprTable :: Vector Int }
  deriving Binary
newtype SymMove s a = SymMove (Vector SymCoord') deriving Binary

-- | Compute the table of smallest representatives for all symmetry classes.
-- The @RawCoord'@ coordinate of that representative is a @Repr@.
-- The table is sorted in increasing order.
symClasses
  :: RawEncoding a {- ^ Coordinate encoding -}
  -> Action s a    {- ^ Symmetry group, including the identity,
                    -   represented by its action on @a@ -}
  -> SymClassTable s a {- ^ Smallest representative -}
symClasses c sym = SymClassTable . U.fromList . fmap unRawCoord $ symClasses' c sym

symClasses' :: forall a s. RawEncoding a -> Action s a -> [RawCoord a]
symClasses' c (Action sym)
  = foldFilter (H.empty :: H.MinHeap (RawCoord a)) (fmap (RawCoord . tsi) [0 .. range c - 1])
  where
    tsi x = (if x `mod` (range c `div` 100) == 0 then traceShowId else id) x
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

symClassTable
  :: Int
  -> SymReprTable s a
  -> SymClassTable s a
symClassTable nSym (SymReprTable s)
  = SymClassTable . U.ifilter (==) $ U.map (`div` nSym) s

symReprTable'
  :: Int -- ^ Number of elements @n@
  -> Int -- ^ Number of symmetries @nSym@
  -> (Int -> [Int]) -- ^ @f x@, symmetrical elements to @x@, including itself
  -> Vector Int
  -- ^ @v@, where @(y, i) = (v ! x) `divMod` nSym@ gives
  -- the representative @y@ of the symmetry class of @x@
  -- and the index of one symmetry mapping @x@ to @y@:
  -- 
  -- > f x !! i == y.
symReprTable' n nSym f
  = U.create $ do
      v <- MU.replicate n (-1)
      forM_ [0 .. n-1] $ \x -> do
        let ys = f x
        y <- MU.read v x
        when (y == -1) .
          forM_ (zip [0 ..] (f x)) $ \(i, x') ->
            MU.write v x' (flatIndex nSym x i)
      return v

-- |
symMoveTable
  :: RawEncoding a
  -> Action s a      {- ^ Symmetry group -}
  -> SymClassTable s a   {- ^ (Sorted) table of representatives -}
  -> (a -> a)        {- ^ Endofunction to encode -}
  -> SymMove s a
symMoveTable enc action@(Action syms) classes f
  = SymMove (U.map move (unSymClassTable classes))
  where
    n = length syms
    move = flat . symCoord enc action classes . f . decode enc . RawCoord
    flat (SymClass c, SymCode s) = flatIndex n c s

symMoveTable'
  :: RawEncoding a
  -> Int -- ^ Symmetry group order
  -> SymReprTable s a
  -> SymClassTable s a
  -> (a -> a)
  -> SymMove s a
symMoveTable' enc nSym reps classes f
  = SymMove (U.map move (unSymClassTable classes))
  where
    move = flat . symCoord' nSym reps classes . encode enc . f . decode enc . RawCoord
    flat (SymClass c, SymCode s) = flatIndex nSym c s

symMove :: SymOrder' -> SymMove s a -> SymClass s a -> (SymClass s a, SymCode s)
symMove n (SymMove v) (SymClass x) = (SymClass y, SymCode i)
  where (y, i) = (v U.! x) `divMod` n

symMove' n v (x, j) = (y, i `composeSym` j)
  where (y, i) = symMove n v x

reprToClass :: SymClassTable s a -> RawCoord a -> SymClass s a
reprToClass (SymClassTable cls) = SymClass . fromJust . flip iFind cls . unRawCoord

-- | Find the representative as the one corresponding to the smallest coordinate
symCoord :: RawEncoding a -> Action s a -> SymClassTable s a
  -> a -> SymCoord s a
symCoord c (Action syms) classes x
  = (reprToClass classes r, SymCode s)
  where
    xSym = [ encode c (s x) | s <- syms ]
    (r, s) = minimumBy (comparing fst) (zip xSym [0 ..])

symCoord' :: Int -> SymReprTable s a -> SymClassTable s a -> RawCoord a -> SymCoord s a
symCoord' nSym (SymReprTable reps) (SymClassTable classes) (RawCoord x)
  = (SymClass r, SymCode i)
  where
    (y, i) = (reps U.! x) `divMod` nSym
    r = fromJust $ iFind r classes
