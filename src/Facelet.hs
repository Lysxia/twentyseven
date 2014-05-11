{- |
   Facelet representation

   Facelets faces are laid out like this:

   @
       U
     L F R B
       D
   @

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

import Misc

import Data.Char
  ( intToDigit )

import Data.Array.Unboxed
  ( UArray
  , elems
  , amap )

-- | Facelet representation as a permutation array (replaced-by)
newtype Facelets = Facelets (UArray Int Int)

type Color = Int

-- | Only represent the colors of facelets
newtype ColorFacelets = ColorFacelets (UArray Int Color)

-- | Bounds of a @Facelets@ array. There are @54 == 6 * 9@ facelets.
boundsF :: (Int, Int)
boundsF = (0, 6 * 9 - 1)

instance Group Facelets where
  iden = Facelets $ idArray boundsF
  inverse (Facelets a) = Facelets $ inverseArray a
  (Facelets b) ? (Facelets c) = Facelets $ composeArray b c

printFacelets :: Facelets -> IO ()
printFacelets (Facelets fl)
  = putStrLn . insertEvery 2 ' ' . concatMap base9 $ elems fl
  where base9 n = map intToDigit [n `div` 9, n `mod` 9]

printColorFacelets :: ColorFacelets -> IO ()
printColorFacelets (ColorFacelets fl)
  = putStrLn . insertEvery 9 ' ' . map colorChar $ elems fl

-- | The color of a facelet
color :: Int -> Color
color = flip div 9

toColorFacelets :: Facelets -> ColorFacelets
toColorFacelets (Facelets c) = ColorFacelets $ amap color c

-- | A color is mapped to a face, indicated by a @Char@:
-- 
-- > map colorChar [0..5] == "ULFRBD"
colorChar :: Color -> Char
colorChar 0 = 'U'
colorChar 1 = 'L'
colorChar 2 = 'F'
colorChar 3 = 'R'
colorChar 4 = 'B'
colorChar 5 = 'D'

-- | Return as a permutation list
toList :: Facelets -> [Int]
toList (Facelets fl) = elems fl

insertEvery :: Int -> a -> [a] -> [a]
insertEvery n x xs =
  x1 ++
  if null x2
    then []
    else [x] ++ insertEvery n x x2
  where (x1, x2) = splitAt n xs

printColor :: Facelets -> IO ()
printColor = printColorFacelets . toColorFacelets

