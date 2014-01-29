module Facelet where

import Data.Char
import Data.Array.Unboxed
import Misc

newtype Cube = Cube (UArray Int Int)
newtype ColorCube = ColorCube (UArray Int Int)

boundsF :: (Int, Int)
boundsF = (1, 6 * 9)

instance Group Cube where
  iden = Cube $ idArray boundsF
  inverse (Cube a) = Cube $ inverseArray a
  (Cube b) ? (Cube c) = Cube $ composeArray b c

instance Show Cube where
  show (Cube fl) = insertEvery 2 ' ' $ concatMap base9 $ map (flip (-) 1)
                                                           $ elems fl
    where base9 n = [intToDigit $ n `div` 9, intToDigit $ n `mod` 9]

instance Show ColorCube where
  show (ColorCube fl) = insertEvery 9 ' ' $ map colorChar $ elems fl

color :: Int -> Int
color = (`div` 9).(flip (-) 1)

toColorCube :: Cube -> ColorCube
toColorCube (Cube c) = ColorCube $ amap color c

colorChar :: Int -> Char
colorChar 0 = 'U'
colorChar 1 = 'L'
colorChar 2 = 'F'
colorChar 3 = 'R'
colorChar 4 = 'B'
colorChar 5 = 'D'
colorChar _ = undefined

toList :: Cube -> [Int]
toList (Cube fl) = elems fl

insertEvery :: Int -> a -> [a] -> [a]
insertEvery n x xs =
  x1 ++
  if null x2
    then []
    else [x] ++ insertEvery n x x2
  where (x1, x2) = splitAt n xs

showColor = show . toColorCube

