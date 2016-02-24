module Rubik.Cube.Facelet.Internal where

import Rubik.Misc

import Control.Applicative
import Control.Monad

import Data.Char ( intToDigit )
import Data.List
import Data.Monoid
import qualified Data.Vector.Unboxed as U

-- | There are @54 == 6 * 9@ facelets.
numFacelets :: Int
numFacelets = 6 * 9

-- | Cube as a permutation of facelets (replaced-by).
--
-- Every facelet is represented as an 'Int' in @[0 .. 54]@.
newtype Facelets = Facelets {
    -- | The underlying 'Vector' of 'Int'.
    fromFacelets :: Vector Int
  } deriving (Eq, Show)

instance Monoid Facelets where
  mempty = Facelets $ idVector numFacelets
  mappend (Facelets b) (Facelets c) = Facelets $ composeVector b c

instance Group Facelets where
  inverse (Facelets a) = Facelets $ inverseVector a

-- | See 'fromFacelets''
fromFacelets' :: Facelets -> [Int]
fromFacelets' = U.toList . fromFacelets

-- | See 'facelets'.
facelets' :: [Int] -> Maybe Facelets
facelets' = facelets . U.fromList

-- | This constructor checks that the input is a permutation of '[0 .. 53]'.
facelets :: Vector Int -> Maybe Facelets
facelets v = do
  guard $ U.length v == numFacelets
       && isPermutationVector v
  return (Facelets v)

-- | Constructor with no safety checks
unsafeFacelets = Facelets
unsafeFacelets' = Facelets . U.fromList

-- | The standard cube colors are the values between @0@ and @5@.
type Color = Int

-- | Cube as a list of facelet colors.
newtype ColorFacelets = ColorFacelets {
    -- | The underlying 'Vector' of 'Color'.
    fromColorFacelets :: Vector Color
  } deriving (Eq, Show)

-- | See 'fromColorFacelets'.
fromColorFacelets' :: ColorFacelets -> [Color]
fromColorFacelets' = U.toList . fromColorFacelets

-- | See 'colorFacelets'.
colorFacelets' :: [Color] -> Maybe ColorFacelets
colorFacelets' = colorFacelets . U.fromList

-- | This constructor checks that only standard colors (in @[0 .. 5]@)
-- are used, that the argument has length @54@ and that the centers
-- are colored in order.
--
-- Note that there may still be more or less than 9 colors of a kind,
-- although that cannot be the case in an actual cube.
colorFacelets :: Vector Color -> Maybe ColorFacelets
colorFacelets v = do
  guard $ U.length v == numFacelets
       && U.all (\c -> 0 <= c && c < 6) v
       && map (v U.!) centerFacelets == [0 .. 5]
  return (ColorFacelets v)

-- | The color of a facelet given its identifier.
colorOf :: Int -> Color
colorOf = (`div` 9)

-- | Remove permutation information.
--
-- If the argument cube can be obtained from the solved cube with the usual moves,
-- then the original permutation can be recovered with 'Cubie.colorFaceletsToCube'.
colorFaceletsOf :: Facelets -> ColorFacelets
colorFaceletsOf = ColorFacelets . U.map colorOf . fromFacelets

-- | A color is mapped to a face, indicated by a @Char@:
--
-- > map colorChar [0..5] == "ULFRBD"
colorChar :: Color -> Char
colorChar = ("ULFRBD" !!)

-- | String listing the permutation of facelets numbered in base 9.
--
-- Base 9 is convenient here because the first digit directly corresponds to a face
-- and the second to the facelet position in that face.
stringOfFacelets :: Facelets -> String
stringOfFacelets
  = intercalate " " . map base9 . U.toList . fromFacelets
  where base9 n = map intToDigit [n `div` 9, n `mod` 9]

-- | String listing the facelet colors.
stringOfColorFacelets :: ColorFacelets -> String
stringOfColorFacelets
  = intercalate " " . chunk 9 . map colorChar . U.toList . fromColorFacelets

-- | Only show the colors of the facelets.
stringOfColorFacelets' :: Facelets -> String
stringOfColorFacelets' = stringOfColorFacelets . colorFaceletsOf

--

-- | Convert a 6-color list of length 54 in any representation which implements 'Eq'
-- to 'ColorFacelets'.
colorFacelets'' :: Eq a => [a] -> Maybe ColorFacelets
colorFacelets'' colors = do
  guard (length colors == numFacelets)
  guard (length (nub centers) == 6)
  colorFacelets' =<< sequence ((`lookup` zip centers [0 .. 5]) <$> colors)
  where
    centers = (colors !!) <$> centerFacelets

--

-- Facelets corresponding to each cubie

-- |
-- @
--   centerFacelets
--   = [ 4,  -- U
--       13, -- L
--       22, -- F
--       31, -- R
--       40, -- B
--       49] -- D
-- @
centerFacelets :: [Int]
centerFacelets = [4, 13 .. 49]

-- | Corner facelets
ulb, ufl, urf, ubr, dlf, dfr, drb, dbl :: [Int]
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

-- | Edge facelets
ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br :: [Int]
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
