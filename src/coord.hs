{-# LANGUAGE ExistentialQuantification #-}
module Coord
  where

import Data.Array.Unboxed
import Data.List
import Misc
import Cubie

-- MaxInt 2^29 = 479001600
type Coord = Int

-- May be generalizable to Bounded or something ?
class Coordinate a where
  encode :: a -> Coord
  decode :: Coord -> a

instance Coordinate Int where
  encode = id
  decode = id

-- Fixed base representation

encodeBase :: Int -> [Int] -> Coord
encodeBase base = foldl1 ((+).(* base))

decodeBase :: Int -> Int -> Coord -> [Int]
decodeBase base size digits = decode' size digits []
  where decode' 0 _ l = l
        decode' s d l = decode' (s-1) (d `div` base) (d `mod` base : l)

-- Factorial radix representation (Mixed radix [n..2])
-- Input list must be a permutation of [1..n]

encodeFact :: [Int] -> Coord
encodeFact l = encode' (length l) l
  where encode' _ [_] = 0
        encode' n l  = (encode' (n-1) $ tail $ subs k (head l) l) * n + k
          where Just k = elemIndex (n-1) l

decodeFact :: Coord -> Int -> [Int]
decodeFact _ 0 = []
decodeFact x n =
  if k == -1
    then (n-1)  : l
    else l !! k : subs k (n-1) l
  where l = decodeFact (x `div` n) (n-1)
        k = (x `mod` n) - 1

-- Bijection between [0..nCk-1] and k-subsets of [0..n-1]
-- see kociemba.org/math/UDSliceCoord.htm

encodeC :: [Int] -> Int -> Coord
encodeC [] _ = 0
encodeC (a : as) n = encode' 0 a as
  where encode' k m []       = sum [m' `choose` k | m' <- [m+1..n-1]]
        encode' k m (a : as) = sum [m' `choose` k | m' <- [m+1..a-1]]
                             + encode' (k + 1) a as

decodeC :: Coord -> Int -> Int -> [Int]
decodeC n k x = decode' n (k - 1) x []
  where decode' n (-1) _ acc = acc
        decode' n k    x acc =
          if x < nCk
            then decode' n' (k - 1) x         (n' : acc)
            else decode' n' k       (x - nCk) acc
          where nCk = n' `choose` k
                n'  = n - 1

--

-- Array instance
listArray' :: Ix i => (i, i) -> [a] -> Array i a
listArray' = listArray

-- x < 8! = 40320
instance Coordinate CornerPermu where
  encode (CornerPermu p) = encodeFact p
  decode = (cpA !)
    where cpA = listArray' (0, 40319) [decode' x | x <- [0..40319]]
          decode' x = CornerPermu $ decodeFact x numCorners

-- x < 12! = 479001600... a bit too much for memory
-- Holds just right in a Haskell Int (< 2^29)
instance Coordinate EdgePermu where
  encode (EdgePermu p) = encodeFact p
  decode x = EdgePermu $ decodeFact x numEdges


-- x < 3^7 = 2187
instance Coordinate CornerOrien where
  encode (CornerOrien o) = encodeBase 3 $ tail o
  -- The first orientation can be deduced from the others in a solvable cube
  decode = (coA !)
    where coA = listArray' (0, 2186) [decode' x | x <- [0..2186]]
          decode' x = CornerOrien $ h : t
            where h = (3 - sum t) `mod` 3
                  t = decodeBase 3 (numCorners - 1) x

-- x < 2^11 = 2048
instance Coordinate EdgeOrien where
  encode (EdgeOrien o) = encodeBase 2 $ tail o
  decode = (eoA !)
    where eoA = listArray' (0, 2047) [decode' x | x <- [0..2047]]
          decode' x = Cubie.EdgeOrien $ h : t
            where h = sum t `mod` 2
                  t = decodeBase 2 (numEdges - 1) x

-- x < 12C4 = 495
instance Coordinate UDSlice where
  encode (UDSlice s) = encodeC s 12
  decode x = UDSlice $ decodeC 12 4 x

--

type MoveF a = a -> a
type MoveTable = UArray (Coord, Int) Coord
data MoveSet = forall a. Coordinate a => MoveSet [MoveF a]

moveTable :: Coordinate a => Int -> [MoveF a] -> MoveTable
moveTable xBound moves = listArray ((0,0), (xBound, n - 1)) l
  where n = length moves
        l = concat [map (encode.($ decode x)) moves | x <- [0..xBound-1]]

moveTable' :: Int -> MoveSet -> MoveTable
moveTable' xBound (MoveSet moves) = moveTable xBound moves
