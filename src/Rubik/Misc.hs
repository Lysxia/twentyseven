{- |
   General functions for the __twentyseven__ project
-}

module Rubik.Misc where

--import Math.Combinatorics.Exact.Binomial ( choose )

import Control.Applicative

import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Generic as G

-- | Convert 2D indices to 1D.
--
-- > \n x y -> x * n + y
flatIndex :: Int -> Int -> Int -> Int
flatIndex n x y = x * n + y

-- * Applicative

zipWith' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
{-# INLINE zipWith' #-}
zipWith' f x y = f <$> x <*> y

sequence' :: Applicative f => [f a] -> f [a]
{-# INLINE sequence' #-}
sequence' = foldr (liftA2 (:)) (pure [])

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

-- | Insert before the @n@-th element.
insert' :: Int -> a -> [a] -> [a]
insert' 0 x l = x : l
insert' n x (h : t) = h : insert' (n-1) x t

-- | If @l@ is a permutation list (replaced-by) of length @n@,
-- @inverseList n l@ is its inverse permutation.
inverseList :: Int -> [Int] -> [Int]
inverseList n l = [fromJust $ elemIndex i l | i <- [0 .. n - 1]]

-- | Backpermute. Substitute every index in the second list with the
-- corresponding element in the first.
composeList :: [a] -> [Int] -> [a]
composeList = map . (!!)

-- | Strict in every element of the list.
listSeq :: [a] -> b -> b
listSeq [] b = b
listSeq (a : as) b = a `seq` listSeq as b

-- * Vectors

-- | Unboxed vectors
type Vector = U.Vector

-- | Permutation of @[0 .. length v]@.
isPermutationVector :: Vector Int -> Bool
isPermutationVector v = all (`U.elem` v) [0 .. U.length v - 1]

-- | Sign of a permutation vector.
signPermutationVector :: Vector Int -> Int
signPermutationVector v =
  length [ (x, y) | x <- [0 .. n - 1],
                    y <- [x + 1 .. n - 1],
                    v U.! x < v U.! y ] `mod` 2
  where n = U.length v

-- | > idVector n == fromList [0 .. n - 1]
idVector :: Int -> Vector Int
idVector = U.enumFromN 0

-- | If @v@ is a permutation,
-- @inverseVector v@ is its inverse permutation.
inverseVector :: Vector Int -> Vector Int
inverseVector u = U.create (do
  v <- MU.new n
  iForM_ u . flip $ MU.write v
  return v)
  where
    n = U.length u
    iForM_ u = U.forM_ (U.indexed u) . uncurry

-- | Permutation composition: @(p . q) x == p (q x)@.
--
-- > composeVector u v ! i == u ! (v ! i)
composeVector :: U.Unbox a => Vector a -> Vector Int -> Vector a
composeVector = U.backpermute

-- * Groups

infixr 8 <>^

-- | Class for groups:
--
-- > a <> (b <> c) == (a <> b) <> c -- Associative property
--
-- > a <> iden == a -- Neutral element
-- > iden <> a == a
--
-- > a <> inverse a == iden -- Inverse
-- > inverse a <> a == iden
--
class Monoid a => Group a where
  inverse :: a -> a

-- | Alias for 'mempty'
iden :: Group a => a
iden = mempty

instance (Group a, Group b) => Group (a, b) where
  inverse (a, b) = (inverse a, inverse b)

-- | Exponentiation, negative indices are supported.
(<>^) :: (Integral int, Group a) => a -> int -> a
_ <>^ 0 = iden
a <>^ 1 = a
a <>^ n
 | n < 0          = inverse a <>^ (-n)
 | n `mod` 2 == 0 = a2
 | otherwise      = a <> a2
 where a2 = a_n_2 <> a_n_2
       a_n_2 = a <>^ (n `div` 2)

-- | Conjugation:
--
-- > s `conjugate` a = inverse s <> a <> s
conjugate, (??) :: Group a => a -> a -> a
conjugate s a = inverse s <> a <> s
(??) = conjugate

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
iFind :: (Integral a, Ord a, G.Vector v a) => a -> v a -> Maybe Int
iFind x v | x < G.head v || G.last v < x = Nothing
iFind x v = find 0 n
  where
    n = G.length v
    find _ 0 = Nothing
    find a m = case compare x (v G.! (a + p)) of
        LT -> find a p
        EQ -> Just (a + p)
        GT -> find (a + p + 1) (m - p - 1)
      where
        s = v G.! a
        t = v G.! (a + m - 1)
        p = fromIntegral $ ((x - s) * (fromIntegral m - 1)) `div` (t - s)

-- | Flipped "if"
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y

-- | Equal sized chunks
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = x1 : chunk n x2
  where (x1, x2) = splitAt n xs

-- | Generalized partition
partition' :: (a -> a -> Bool) -> [a] -> [[a]]
partition' (==) [] = []
partition' (==) (a : as) = (a : as') : partition' (==) as''
  where (as', as'') = partition (== a) as

tagOf :: tag a b -> tag' a b' -> tag a b
tagOf = const

asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const

proxyUnwrap :: proxy (f a) -> Proxy a
proxyUnwrap _ = Proxy

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
