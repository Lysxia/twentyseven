module Misc where

import Data.Maybe
import Data.List
import Data.Array.IArray
import Data.Array.Unboxed

-- Lists

rotate :: Int -> [a] -> [a]
rotate n l = l2 ++ l1
  where (l1, l2) = splitAt n l

subs 0 x (a : as) = x : as
subs n x (a : as) = a : subs (n-1) x as

-- n = length l
inverseList n l = [fromJust $ elemIndex i l | i <- [0..n - 1]]

composeList = map . (!!)

-- Arrays

idArray :: (IArray a i, Ix i) => (i, i) -> a i i
idArray r = listArray r $ range r

inverseArray c = array (bounds c) $ map (\(x,y) -> (y,x)) $ assocs c

composeArray :: (IArray a i, Ix i) => a i i -> a i i -> a i i
composeArray a b = ixmap (bounds b) (b !) a

-- Groups

class Group a where
  iden :: a
  inverse :: a -> a
  (?) :: a -> a -> a

instance (Group a, Group b) => Group (a, b) where
  iden = (iden, iden)
  inverse (a, b) = (inverse a, inverse b)
  (a1, b1) ? (a2, b2) = (a1 ? a2, b1 ? b2)

(?^) :: Group a => a -> Int -> a
_ ?^ 0 = iden
a ?^ 1 = a
a ?^ n | n > 0     = a ? (a ?^ (n-1))
       | otherwise = (inverse a) ?^ (-n)

(??) :: Group a => a -> a -> a
s ?? a = inverse s ? a ? s

-- Combinatorics

fact 0 = 1
fact n = n * fact (n - 1)

choose :: Int -> Int -> Int
choose = \n k -> if k < 0 then 0 else c !! n ! k
  where c = [listArray (0,n) $ line n | n <- [0..]] :: [UArray Int Int]
        line n = [if k == 0 || k == n
                    then 1
                    else let cn = c !! (n - 1) in
                         cn ! k + cn ! (k - 1)    | k <- [0..n]]
 
