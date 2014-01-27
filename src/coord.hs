module Coord
  where

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

-- The first orientation can be deduced from the others in a solvable cube

eCornerP :: Cubie.CornerPermu -> Coord
eCornerP (Cubie.CornerPermu p) = encodeFact p

eCornerO :: Cubie.CornerOrien -> Coord
eCornerO (Cubie.CornerOrien o) = encode 3 $ tail $ map (`mod` 3) o

eEdgeP :: Cubie.EdgePermu -> Coord
eEdgeP (Cubie.EdgePermu p) = encodeFact p

eEdgeO :: Cubie.EdgeOrien -> Coord
eEdgeO (Cubie.EdgeOrien o) = encode 2 $ tail $ map (`mod` 2) o

dCornerP :: Coord -> Cubie.CornerPermu
dCornerP x = Cubie.CornerPermu $ decodeFact x Cubie.numCorners

dCornerO :: Coord -> Cubie.CornerOrien
dCornerO x = Cubie.CornerOrien $ h : t
  where h = (3 - sum t) `mod` 3
        t = decode 3 (Cubie.numCorners - 1) x

dEdgeP :: Coord -> Cubie.EdgePermu
dEdgeP x = Cubie.EdgePermu $ decodeFact x Cubie.numEdges

dEdgeO :: Coord -> Cubie.EdgeOrien
dEdgeO x = Cubie.EdgeOrien $ h : t
  where h = sum t `mod` 2
        t = decode 2 (Cubie.numEdges - 1) x
