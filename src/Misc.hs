{- |
   General functions for the __twentyseven__ project
-}

module Misc where

--import Math.Combinatorics.Exact.Binomial ( choose )

import Data.Maybe
import Data.List
import Data.Array.IArray
import Data.Array.Unboxed

-- * Lists

-- | Rotation: @rotate 3 [1 .. 7] == [4 .. 7] ++ [1 .. 3]@
rotate :: Int -> [a] -> [a]
rotate n l = l2 ++ l1
  where (l1, l2) = splitAt n l

-- | Substitute the @n@-th element
subs :: Int -> a -> [a] -> [a]
subs 0 x (a : as) = x : as
subs n x (a : as) = a : subs (n - 1) x as

-- | If @l@ is a permutation list of length @n@,
-- @inverseList n l@ is its inverse permutation.
inverseList :: Int -> [Int] -> [Int]
inverseList n l = [fromJust $ elemIndex i l | i <- [0 .. n - 1]]

composeList :: [Int] -> [Int] -> [Int]
composeList = map . (!!)

-- * Arrays

-- |
-- > idArray r ! i = i
idArray :: (IArray a i, Ix i) => (i, i) -> a i i
idArray r = listArray r $ range r

-- | If @c@ is a permutation, @inverseArray c@ is its inverse permutation
-- (replaced-by representation)
inverseArray :: (IArray a i, Ix i) => a i i -> a i i
inverseArray c = array (bounds c) $ map (\(x, y) -> (y, x)) $ assocs c

-- | Permutation composition
composeArray :: (IArray a i, Ix i) => a i i -> a i i -> a i i
composeArray a b = ixmap (bounds b) (b !) a

-- * Groups

infixl 7 ?
infixr 8 ?^

-- | Class for groups
--
-- > a ? (b ? c) == (a ? b) ? c -- Associative property
--
-- > a ? iden == a -- Neutral element
-- > iden ? a == a
--
-- > a ? inverse a == iden -- Inverse
-- > inverse a ? a == iden
--
class Group a where
  iden :: a
  inverse :: a -> a
  (?) :: a -> a -> a

instance (Group a, Group b) => Group (a, b) where
  iden = (iden, iden)
  inverse (a, b) = (inverse a, inverse b)
  (a1, b1) ? (a2, b2) = (a1 ? a2, b1 ? b2)

-- | Exponentiation. Negative indices are supported.
(?^) :: (Integral int, Group a) => a -> int -> a
_ ?^ 0 = iden
a ?^ 1 = a
a ?^ n
 | n < 0          = inverse a ?^ (-n)
 | n `mod` 2 == 0 = a2
 | otherwise      = a ? a2
 where a2 = a_n_2 ? a_n_2
       a_n_2 = a ?^ (n - 1)

-- | Conjugation: @s ?? a = inverse s ? a ? s@
(??) :: Group a => a -> a -> a
s ?? a = inverse s ? a ? s

-- * Combinatorics

-- | Factorial
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- | Binomial coefficient:
-- @choose n k == fact n `div` (fact k) * (fact (n - k))@
choose :: Int -> Int -> Int
choose = \n k -> if k < 0 then 0 else c !! n ! k
  where c = [listArray (0,n) $ line n | n <- [0..]] :: [UArray Int Int]
        line n = do
          k <- [0..n]
          if k == 0 || k == n
            then return 1
            else let cn = c !! (n - 1) in
                 return $ cn ! k + cn ! (k - 1)
