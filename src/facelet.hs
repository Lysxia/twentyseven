module Facelet where

import Data.Array.Unboxed

newtype Cube = Cube (UArray Int Int)

color :: Int -> Int
color = (`div` 9).(flip (-) 1)

colorChar :: Int -> Char
colorChar 0 = 'U'
colorChar 1 = 'L'
colorChar 2 = 'F'
colorChar 3 = 'R'
colorChar 4 = 'B'
colorChar 5 = 'D'
colorChar _ = undefined

insertEvery :: Int -> a -> [a] -> [a]
insertEvery n x xs = x1 ++
  if null x2
    then []
    else [x] ++ insertEvery n x x2
  where (x1, x2) = splitAt n xs

instance Show Cube where
  show (Cube fl) = insertEvery 9 ' ' $ map (colorChar.color) $ elems fl

rangeF :: (Int, Int)
rangeF = (1, 6 * 9)

id :: Cube
id = Cube $ listArray rangeF [1..6 * 9]

compose :: Cube -> Cube -> Cube
compose (Cube a) (Cube b) =
  Cube $ listArray rangeF [b!i | i <- elems a]


