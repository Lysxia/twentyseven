module Misc where

import Data.Array.IArray
import Data.Array.Unboxed

-- Lists

rotate :: Int -> [a] -> [a]
rotate n l = l2 ++ l1
  where (l1, l2) = splitAt n l

subs 0 x (a : as) = x : as
subs n x (a : as) = a : subs (n-1) x as

composeList = map . (!!)

-- Arrays

idArray :: (IArray a i, Ix i) => (i, i) -> a i i
idArray r = listArray r $ range r

composeArray :: (IArray a i, Ix i) => a i i -> a i i -> a i i
composeArray a b = ixmap (bounds b) (b !) a

-- Groups

class Group a where
  iden :: a
  compose :: a -> a -> a

instance (Group a, Group b) => Group (a, b) where
  iden = (iden, iden)
  (a1, b1) `compose` (a2, b2) = (a1 `compose` a2, b1 `compose` b2)

gexp :: Group a => a -> Int -> a
gexp _ 0 = iden
gexp a 1 = a
gexp a n = a `compose` gexp a (n-1)

conjugate :: Group a => a -> a -> a
conjugate s a = s `compose` a `compose` s

-- Combinatorics

choose :: Int -> Int -> Int
choose = \n k -> if k < 0 then 0 else c !! n ! k
  where c = [listArray (0,n) $ line n | n <- [0..]] :: [UArray Int Int]
        line n = [if k == 0 || k == n
                    then 1
                    else let cn = c !! (n - 1) in
                         cn ! k + cn ! (k - 1)    | k <- [0..n]]
 
