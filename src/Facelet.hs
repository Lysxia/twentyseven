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
  facelets,
  fromFacelets,

  -- * Colors
  Color,
  colorOf,
  colorChar,

  -- * Color list
  ColorFacelets,
  colorFacelets,
  fromColorFacelets,
  colorFaceletsOf,

  -- * List conversions
  fromFacelets',
  facelets',
  fromColorFacelets',
  colorFacelets',
  normalize,

  -- * Pretty conversion
  stringOfFacelets,
  stringOfColorFacelets,
  stringOfColorFacelets',
  
  -- * Unsafe
  unsafeFacelets,

  -- * Facelets corresponding to each cubie
  -- $mnemonic 

  -- ** Centers
  centerFacelets,

  -- ** Corners
  cornerFacelets,
  ulb, ufl, urf, ubr, dlf, dfr, drb, dbl,

  -- ** Edges
  edgeFacelets,
  ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br
  ) where

import Misc

import Control.Applicative
import Control.Monad

import Data.Char ( intToDigit )
import Data.List
import qualified Data.Vector.Unboxed as U

-- | There are @54 == 6 * 9@ facelets.
numFacelets :: Int
numFacelets = 6 * 9

-- | Cube as a permutation of facelets (replaced-by).
newtype Facelets = Facelets { fromFacelets :: Vector Int }
  deriving (Eq, Show)

instance Group Facelets where
  iden = Facelets $ idVector numFacelets
  inverse (Facelets a) = Facelets $ inverseVector a
  (Facelets b) ? (Facelets c) = Facelets $ composeVector b c

-- |
fromFacelets' :: Facelets -> [Int]
fromFacelets' = U.toList . fromFacelets

-- | Conversion failure if the argument is not a permutation of size @54@.
facelets' :: [Int] -> Maybe Facelets
facelets' = facelets . U.fromList

-- |
facelets :: Vector Int -> Maybe Facelets
facelets = (Facelets <$>) . mfilter check . Just
  where check v = U.length v == numFacelets && isPermutationVector v

-- | This is the raw constructor of @Facelet@.
-- No check is performed.
unsafeFacelets = Facelets

-- | The standard cube colors are the values between @0@ and @5@.
type Color = Int

-- | Cube as a list of facelet colors.
newtype ColorFacelets = ColorFacelets { fromColorFacelets :: Vector Color }
  deriving (Eq, Show)

-- |
fromColorFacelets' :: ColorFacelets -> [Color]
fromColorFacelets' = U.toList . fromColorFacelets

-- | This constructor checks that only standard colors (in @[0 .. 5]@) are used,
-- that the argument has length @54@ and that the centers are colored in order.
--
-- Note that there may be more than or less than 9 colors of a kind,
-- although that cannot be the case in an actual cube.
colorFacelets' :: [Color] -> Maybe ColorFacelets
colorFacelets' = colorFacelets . U.fromList

-- |
colorFacelets :: Vector Color -> Maybe ColorFacelets
colorFacelets = (ColorFacelets <$>) . mfilter check . Just
  where check v = U.length v == numFacelets
               && U.all (\c -> 0 <= c && c < 6) v
               && map (v U.!) centerFacelets == [0 .. 5]

-- | The color of a facelet.
colorOf :: Int -> Color
colorOf = (`div` 9)

-- | Remove permutation information.
-- If argument cube can be obtained from the solved cube with the usual moves,
-- then the original permutation can be recovered with @Cubie.FaceletsOf@.
colorFaceletsOf :: Facelets -> ColorFacelets
colorFaceletsOf = ColorFacelets . U.map colorOf . fromFacelets

-- | A color is mapped to a face, indicated by a @Char@:
-- 
-- > map colorChar [0..5] == "ULFRBD"
colorChar :: Color -> Char
colorChar = ("ULFRBD" !!)

stringOfFacelets :: Facelets -> String
stringOfFacelets
  = intercalate " " . map base9 . U.toList . fromFacelets
  where base9 n = map intToDigit [n `div` 9, n `mod` 9]

stringOfColorFacelets :: ColorFacelets -> String
stringOfColorFacelets
  = intercalate " " . chunk 9 . map colorChar . U.toList . fromColorFacelets

stringOfColorFacelets' :: Facelets -> String
stringOfColorFacelets' = stringOfColorFacelets . colorFaceletsOf

--

-- | Convert a 54-color list in any representation which implements @Eq@
-- to @ColorFacelets@.
normalize :: Eq a => [a] -> Maybe ColorFacelets
normalize colors = do
  guard (length colors == numFacelets)
  guard (length (nub centers) == 6)
  colorFacelets' =<< sequence ((`lookup` zip centers [0 .. 5]) <$> colors)
  where
    centers = (colors !!) <$> centerFacelets

--

-- Facelets corresponding to each cubie

-- $mnemonic
-- The first letter in the name of a cubie is
-- the color of its reference facelet
-- (see <http://kociemba.org/math/cubielevel.htm>).
--
-- Corner colors are given in clockwise order.
--
-- Corners are lexicographically ordered
-- (@U>L>F>R>B>D@).
-- 
-- Edges are gathered by horizontal slices (@U, D, UD@).
--

-- | > centerFacelets = [4, 13 .. 49] -- by 9
centerFacelets :: [Int]
centerFacelets = [4, 13 .. 49]

ulb = [ 0,  9, 38]
ufl = [ 6, 18, 11]
urf = [ 8, 27, 20]
ubr = [ 2, 36, 29]
dlf = [45, 17, 24]
dfr = [47, 26, 33]
drb = [53, 35, 42]
dbl = [51, 44, 15]

-- | > cornerFacelets = [ulb, ufl, urf, ubr, dlf, dfr, drb, dbl]
cornerFacelets :: [[Int]]
cornerFacelets = [ulb, ufl, urf, ubr, dlf, dfr, drb, dbl]

ul = [ 3, 10]
uf = [ 7, 19]
ur = [ 5, 28]
ub = [ 1, 37]
dl = [48, 16]
df = [46, 25]
dr = [50, 34]
db = [52, 43]
fl = [21, 14]
fr = [23, 30]
bl = [41, 12]
br = [39, 32]

-- | > edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]
edgeFacelets :: [[Int]]
edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]

--


