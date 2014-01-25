module Facelet (
    FaceletCube (),
    idFacelet,
    composeFacelet,
  )
  where

import Data.Array.Unboxed

newtype FaceletCube = FaceletCube (UArray Int Int)

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

instance Show FaceletCube where
  show (FaceletCube fl) = insertEvery 9 ' ' $ map (colorChar.color) $ elems fl

rangeFacelet :: (Int, Int)
rangeFacelet = (1, 6 * 9)

idFacelet :: FaceletCube
idFacelet = FaceletCube $ listArray rangeFacelet [1..6 * 9]

composeFacelet :: FaceletCube -> FaceletCube -> FaceletCube
composeFacelet (FaceletCube a) (FaceletCube b) =
  FaceletCube $ listArray rangeFacelet [b!i | i <- elems a]


