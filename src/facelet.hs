{- |
   Facelet representation

   A Rubik's cube is a permutation of facelets numbered as follows:

   @
                0  1  2
                3  4  5
                6  7  8

      9 10 11  18 19 20  27 28 29  36 37 38
     12 13 14  21 22 23  30 31 32  39 40 41
     15 16 17  24 25 26  33 34 35  42 43 44

               45 46 47
               48 49 50
               51 52 53
   @
  
-}

module Facelet where

import Data.Char
import Data.Array.Unboxed
import Misc

newtype Cube = Cube (UArray Int Int)
newtype ColorCube = ColorCube (UArray Int Int)

boundsF :: (Int, Int)
boundsF = (0, 6 * 9 - 1)

instance Group Cube where
  iden = Cube $ idArray boundsF
  inverse (Cube a) = Cube $ inverseArray a
  (Cube b) ? (Cube c) = Cube $ composeArray b c

printCube :: Cube -> IO ()
printCube (Cube fl)
  = putStrLn . insertEvery 2 ' ' . concatMap base9 $ elems fl
  where base9 n = map intToDigit [n `div` 9, n `mod` 9]

printColorCube :: ColorCube -> IO ()
printColorCube (ColorCube fl)
  = putStrLn . insertEvery 9 ' ' . map colorChar $ elems fl

color :: Int -> Int
color = flip div 9

toColorCube :: Cube -> ColorCube
toColorCube (Cube c) = ColorCube $ amap color c

colorChar :: Int -> Char
colorChar 0 = 'U'
colorChar 1 = 'L'
colorChar 2 = 'F'
colorChar 3 = 'R'
colorChar 4 = 'B'
colorChar 5 = 'D'

toList :: Cube -> [Int]
toList (Cube fl) = elems fl

insertEvery :: Int -> a -> [a] -> [a]
insertEvery n x xs =
  x1 ++
  if null x2
    then []
    else [x] ++ insertEvery n x x2
  where (x1, x2) = splitAt n xs

printColor :: Cube -> IO ()
printColor = printColorCube . toColorCube

