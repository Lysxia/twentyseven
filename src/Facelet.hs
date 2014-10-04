{- |
   Facelet representation

   Facelets faces are unfolded and laid out like this:

   @
       U
     L F R B
       D
   @

   Faces (or colors) are ordered @U, L, F, R, B, D@.

   A Rubik's cube is a permutation of facelets numbered as follows:

   >            0  1  2
   >            3  4  5
   >            6  7  8
   > 
   >  9 10 11  18 19 20  27 28 29  36 37 38
   > 12 13 14  21 22 23  30 31 32  39 40 41
   > 15 16 17  24 25 26  33 34 35  42 43 44
   > 
   >           45 46 47
   >           48 49 50
   >           51 52 53

-}

module Facelet (
  -- * Facelet permutation
  numFacelets,
  Facelets,
  fromFacelets,
  facelets,

  -- * Colors
  Color,
  colorOf,
  colorChar,

  -- * Color list
  ColorFacelets,
  fromColorFacelets,
  colorFacelets,
  toColorFacelets,

  -- * Vector conversions
  fromFacelets',
  facelets',
  fromColorFacelets',
  colorFacelets',

  -- * Pretty conversion
  stringOfFacelets,
  stringOfColorFacelets,
  stringOfColorFacelets',
  
  -- * Unsafe
  unsafeFacelets')
  where

import Misc

import Data.Char
  ( intToDigit )
import Data.List
import qualified Data.Vector.Unboxed as U

-- | There are @54 == 6 * 9@ facelets.
numFacelets :: Int
numFacelets = 6 * 9

-- | Cube as a permutation of facelets (replaced-by).
newtype Facelets = Facelets { fromFacelets' :: Vector Int }

instance Group Facelets where
  iden = Facelets $ idVector numFacelets
  inverse (Facelets a) = Facelets $ inverseVector a
  (Facelets b) ? (Facelets c) = Facelets $ composeVector b c

-- |
fromFacelets :: Facelets -> [Int]
fromFacelets = U.toList . fromFacelets'

-- | Conversion failure if the argument is not a permutation of size @54@.
facelets :: [Int] -> Maybe Facelets
facelets = facelets' . U.fromList

-- |
facelets' :: Vector Int -> Maybe Facelets
facelets' v
  | isCube v = Just (Facelets v)
  | otherwise = Nothing
  where isCube v = isPermutationVector v && U.length v == numFacelets

-- | This is the raw constructor of @Facelet@.
-- No check is performed.
unsafeFacelets' = Facelets

-- | The standard cube colors are the values between @0@ and @5@.
type Color = Int

-- | Cube as a list of facelet colors.
newtype ColorFacelets = ColorFacelets { fromColorFacelets' :: Vector Color }

-- |
fromColorFacelets :: ColorFacelets -> [Color]
fromColorFacelets = U.toList . fromColorFacelets'

-- | This constructor checks that only standard colors (in @[0 .. 5]@) are used
-- and that the argument has length @54@.
--
-- Note that there may be more than or less than 9 colors of a kind,
-- although that cannot be the case in an actual cube.
colorFacelets :: [Color] -> Maybe ColorFacelets
colorFacelets = colorFacelets' . U.fromList

-- |
colorFacelets' :: Vector Color -> Maybe ColorFacelets
colorFacelets' v
  | check v = Just (ColorFacelets v)
  | otherwise = Nothing
  where check v = U.all (\c -> 0 <= c && c < 6) v && U.length v == numFacelets

-- | The color of a facelet.
colorOf :: Int -> Color
colorOf = (`div` 9)

-- | Remove permutation information.
-- If the original cube can be obtained from the solved cube
-- with the usual moves, then that permutation can be recovered
-- with @Cubie.toFacelet@.
toColorFacelets :: Facelets -> ColorFacelets
toColorFacelets (Facelets c) = ColorFacelets $ U.map colorOf c

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

stringOfFacelets :: Facelets -> String
stringOfFacelets (Facelets fl)
  = intercalate " " . map base9 $ U.toList fl
  where base9 n = map intToDigit [n `div` 9, n `mod` 9]

stringOfColorFacelets :: ColorFacelets -> String
stringOfColorFacelets (ColorFacelets fl)
  = intercalate " " . chunk 9 . map colorChar $ U.toList fl

stringOfColorFacelets' :: Facelets -> String
stringOfColorFacelets' = stringOfColorFacelets . toColorFacelets

