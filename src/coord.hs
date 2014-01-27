module Coord
  where

import Data.Array.IArray 
import Data.List
import qualified Cubie
import Misc

type Coord = Int

-- Fixed base representation

encode :: Int -> [Int] -> Coord
encode base = foldl1 ((+).(* base))

decode :: Int -> Int -> Coord -> [Int]
decode base size digits = decode' size digits []
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
    then (n-1) : l
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

eCornerP :: Cubie.CornerPermu -> Coord
eCornerP (Cubie.CornerPermu p) = encodeFact p

eEdgeP :: Cubie.EdgePermu -> Coord
eEdgeP (Cubie.EdgePermu p) = encodeFact p

eCornerO :: Cubie.CornerOrien -> Coord
eCornerO (Cubie.CornerOrien o) = encode 3 $ tail o

eEdgeO :: Cubie.EdgeOrien -> Coord
eEdgeO (Cubie.EdgeOrien o) = encode 2 $ tail o

-- Array instance
listArray' :: Ix i => (i, i) -> [a] -> Array i a
listArray' = listArray

-- x <- [0..8!-1]
dCornerP :: Coord -> Cubie.CornerPermu
dCornerP = (cpA !)
  where cpA = listArray' (0, 40319) [dCornerP' x | x <- [0..40319]]
        dCornerP' x = Cubie.CornerPermu $ decodeFact x Cubie.numCorners

-- The first orientation can be deduced from the others in a solvable cube
-- x <- [0..3^7-1]
dCornerO :: Coord -> Cubie.CornerOrien
dCornerO = (coA !)
  where coA = listArray' (0, 2186) [dCornerO' x | x <- [0..2186]]
        dCornerO' x = Cubie.CornerOrien $ h : t
          where h = (3 - sum t) `mod` 3
                t = decode 3 (Cubie.numCorners - 1) x

-- x <- [0..12!-1]
-- 12! = 479001600... a bit too much
dEdgeP :: Coord -> Cubie.EdgePermu
dEdgeP x = Cubie.EdgePermu $ decodeFact x Cubie.numEdges

-- x <- [0..2^11-1]
dEdgeO :: Coord -> Cubie.EdgeOrien
dEdgeO = (eoA !)
  where eoA = listArray' (0, 2047) [dEdgeO' x | x <- [0..2047]]
        dEdgeO' x = Cubie.EdgeOrien $ h : t
          where h = sum t `mod` 2
                t = decode 2 (Cubie.numEdges - 1) x

-- x <- [0..47]
-- 2 * 4 * 2 * 3 = 48
-- 2 * 4 * 2 = 16
eSym :: Coord -> Cubie.Cube
eSym = (es !)
  where es = listArray' (0, 47) [eSym' x | x <- [0..47]]
        eSym' x =   (Cubie.surf3Cubie `gexp` x1)
          `compose` (Cubie.sf2Cubie   `gexp` x2)
          `compose` (Cubie.su4Cubie   `gexp` x3)
          `compose` (Cubie.slr2Cubie  `gexp` x4)
          where x4 =  x          `mod` 2
                x3 = (x `div` 2) `mod` 4
                x2 = (x `div` 8) `mod` 2
                x1 =  x `div` 16 -- < 3
