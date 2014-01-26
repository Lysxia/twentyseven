module Coord
  where

import Data.Array.Unboxed
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
  where encode' n [_] = 0
        encode' n l  = (encode' (n-1) $ tail $ subs k (head l) l) * n + k
          where Just k = elemIndex n l

decodeFact :: Coord -> Int -> [Int]
decodeFact _ 1 = [1]
decodeFact x n =
  if k == -1
    then n : l
    else l !! k : subs k n l
  where l = decodeFact (x `div` n) (n-1)
        k = (x `mod` n) - 1

-- The first orientation can be deduced from the others in a solvable cube

eCornerP :: Cubie.CornerPermu -> Coord
eCornerP (Cubie.CornerPermu p) = encodeFact $ elems p

eCornerO :: Cubie.CornerOrien -> Coord
eCornerO (Cubie.CornerOrien o) = encode 3 $ tail $ elems o

eEdgeP :: Cubie.EdgePermu -> Coord
eEdgeP (Cubie.EdgePermu p) = encodeFact $ elems p

eEdgeO :: Cubie.EdgeOrien -> Coord
eEdgeO (Cubie.EdgeOrien o) = encode 2 $ tail $ elems o

dCornerP :: Coord -> Cubie.CornerPermu
dCornerP x = Cubie.CornerPermu $ listArray Cubie.boundsC $ decodeFact x (snd Cubie.boundsC)

dCornerO :: Coord -> Cubie.CornerOrien
dCornerO x = Cubie.CornerOrien $ listArray Cubie.boundsC $ h : t
  where h = (3 - sum t) `mod` 3
        t = decode 3 (snd Cubie.boundsC - 1) x

dEdgeP :: Coord -> Cubie.EdgePermu
dEdgeP x = Cubie.EdgePermu $ listArray Cubie.boundsE $ decodeFact x (snd Cubie.boundsE)

dEdgeO :: Coord -> Cubie.EdgeOrien
dEdgeO x = Cubie.EdgeOrien $ listArray Cubie.boundsE $ h : t
  where h = sum t `mod` 2
        t = decode 2 (snd Cubie.boundsE - 1) x
