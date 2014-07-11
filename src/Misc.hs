{- |
   General functions for the __twentyseven__ project
-}

module Misc where

--import Math.Combinatorics.Exact.Binomial ( choose )

import Data.Maybe
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- * Lists

-- | Rotation:
--
-- > rotate 3 [1,2,3,4,5,6,7] == [4,5,6,7] ++ [1,2,3]
rotate :: Int -> [a] -> [a]
rotate n l = l2 ++ l1
  where (l1, l2) = splitAt n l

-- | Substitute the @n@-th element.
subs :: Int -> a -> [a] -> [a]
subs 0 x (a : as) = x : as
subs n x (a : as) = a : subs (n - 1) x as

-- | Insert before the n-th element.
insert' :: Int -> a -> [a] -> [a]
insert' 0 x l = x : l
insert' n x (h : t) = h : insert' (n-1) x t

-- | If @l@ is a permutation list (replaced-by) of length @n@,
-- @inverseList n l@ is its inverse permutation.
inverseList :: Int -> [Int] -> [Int]
inverseList n l = [fromJust $ elemIndex i l | i <- [0 .. n - 1]]

composeList :: [Int] -> [Int] -> [Int]
composeList = map . (!!)

-- * Vectors
type Vector = U.Vector

idVector :: (Num a, Enum a, MU.Unbox a) => Int -> Vector a
idVector = U.enumFromN 0

-- | If @v@ is a permutation (replaced-by),
-- @inverseVector v@ is its inverse permutation.
inverseVector :: Vector Int -> Vector Int
inverseVector u = U.create (do
  v <- MU.new n
  iForM_ u . flip $ MU.write v
  return v)
  where
    n = U.length u
    iForM_ u = U.forM_ (U.indexed u) . uncurry

-- | Permutation composition, left to right: @(p . q) x == q (p x)@.
--
-- > composeVector u v ! i == v ! (u ! i)
composeVector :: Vector Int -> Vector Int -> Vector Int
composeVector = U.backpermute

-- * Groups

infixl 7 ?
infixr 8 ?^

-- | Class for groups:
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

-- | Exponentiation, negative indices are supported.
(?^) :: (Integral int, Group a) => a -> int -> a
_ ?^ 0 = iden
a ?^ 1 = a
a ?^ n
 | n < 0          = inverse a ?^ (-n)
 | n `mod` 2 == 0 = a2
 | otherwise      = a ? a2
 where a2 = a_n_2 ? a_n_2
       a_n_2 = a ?^ (n `div` 2)

-- | Conjugation:
--
-- > s ?? a = inverse s ? a ? s
(??) :: Group a => a -> a -> a
s ?? a = inverse s ? a ? s

-- * Combinatorics

-- | Factorial
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- | Binomial coefficient:
-- 
-- > choose n k == fact n `div` (fact k) * (fact (n - k))
choose :: Int -> Int -> Int
choose = \n k -> if k < 0 then 0 else c !! n U.! k
  where c = [U.fromList $ line n | n <- [0..]]
        line n = do
          k <- [0..n]
          if k == 0 || k == n
            then return 1
            else let cn = c !! (n - 1) in
                 return $ cn U.! k + cn U.! (k - 1)

-- | Interpolation search for @Int@
iFind :: Int -> Vector Int -> Maybe Int
iFind x v | x < U.head v || U.last v < x = Nothing
iFind x v = find 0 (n - 1)
  where
    n = U.length v
    find _ 0 = Nothing
    find a m = case compare x (v U.! (a + p)) of
        LT -> find a (p - 1)
        EQ -> Just (a + p)
        GT -> find (a + p + 1) (m - p)
      where
        s = v U.! a
        t = v U.! (a + m)
        p = ((x - s) * m) `div` (t - s)

bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y
