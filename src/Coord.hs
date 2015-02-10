{- |
   Encoding cube projections as @Int@ coordinates.

   Explicit dictionary passing style:
   using a class would require explicit type annotations /anyway/.
-}

{-# LANGUAGE ViewPatterns #-}
module Coord (
  -- * Dictionaries
  Coord,
  Coordinate (..),

  -- ** Instances
  -- | The number of elements of every set is given.
  coordCornerPermu,
  coordCornerOrien,
  coordEdgePermu,
  coordEdgeOrien,

  coordUDSlice,
  coordUDSlicePermu2,
  coordUDEdgePermu2,

  -- * Table building
  Endo,
  endoVector,

  cubeActionToEndo,

  moveTable,

  -- * Miscellaneous
  checkCoord,

  -- * Helper
  -- | Helper functions to define the dictionaries

  -- ** Fixed base
  encodeBase,
  decodeBase,

  -- ** Factorial radix
  encodeFact,
  decodeFact,

  -- ** Binomial enumeration
  -- $binom
  encodeCV,
  decodeCV,
  ) where

import Cubie
import Misc

import Control.Applicative
import Control.Arrow
import Control.Monad.ST.Safe

import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- MaxInt 2^29 = 479001600
-- | Encoding to an efficient datatype
-- for which it is possible to build tables
-- instead of computing functions.
type Coord = Int

-- | Encoding dictionary.
--
-- Probably synonymous with instances for both
-- @('Enum' a, 'Bounded' a)@.
--
-- > inRange (range d) $ encode x
-- > encode . decode == id
-- > decode . encode == id
--
-- A special constructor for dictionaries of product types
-- is particularly useful to create tables of functions
-- if their actions on every projection are independent.
--
data Coordinate a = Coord {
    range  :: Coord, {- ^ Number of elements that can be converted.
                         Their values are to lie in @[0 .. range c - 1]@. -}
    encode :: a -> Coord,
    decode :: Coord -> a }

-- Fixed base representation

-- | If
-- @all (`elem` [0 .. b-1]) v@
-- then @v@ is the base @b@ representation of
-- @encode b v@
-- such that its least significant digit is @head v@.
--
-- For any @n@, @encodeBase b@ is a bijection from lists of length @n@
-- with elements in @[0 .. b-1]@ to @[0 .. b^n - 1]@
encodeBase :: Int -> [Int] -> Coord
encodeBase b = foldr1 (\x y -> x + b * y)

-- | Vector version of 'encodeBase'.
encodeBaseV :: Int -> Vector Int -> Coord
encodeBaseV b = U.foldr1' (\x y -> x + b * y)

-- | @len@ is the length of the resulting vector
--
-- > encodeBase b . decodeBase b len == id
-- > decodeBase b len . encodeBase b == id
--
decodeBase :: Int -> Int -> Coord -> [Int]
decodeBase b len = take len . unfoldr (\x -> Just (x `modDiv` b))
  where modDiv = ((.).(.)) (\(x,y) -> (y,x)) divMod

-- Factorial radix representation

-- | Input list must be a permutation of @[0 .. n-1]@.
--
-- @encodeFact@ is a bijection between permutations of @[0 .. n-1]@
-- and @[0 .. fact n - 1]@.
encodeFact :: Int -> [Int] -> Coord
encodeFact n = encode' n . mixedRadix n
  where
    mixedRadix 1 [_] = []
    mixedRadix n l = k : digits
      where
        Just k = elemIndex (n - 1) l
        digits = mixedRadix (n - 1) $ delete (n - 1) l
    encode' _ [] = 0
    encode' n (h : t) = h + n * encode' (n - 1) t

-- | Inverse of 'encodeFact'.
--
-- > encodeFact n . decodeFact n == id
-- > decodeFact n . encodeFact n == id
--
decodeFact :: Int -> Coord -> [Int]
decodeFact 0 _ = []
decodeFact 1 _ = [0]
decodeFact n x = insert' k (n - 1) l
  where (l, k) = decodeFact (n - 1) `first` (x `divMod` n)

-- $binom
-- Bijection between @[0 .. choose n (k-1)]@
-- and @k@-combinations of @[0 .. n-1]@.

-- | > cSum k z == sum [y `choose` k | y <- [k .. z-1]]
--
-- requires @k < cSum_mMaz@ and @z < cSum_nMaz@.
cSum :: Int -> Int -> Int
cSum = \k z -> v U.! (k * n + z)
  where
    cSum' k z = sum [y `choose` k | y <- [k .. z-1]]
    v = U.generate (n * m) (uncurry cSum' . (`divMod` n))
    m = cSum_mMax
    n = cSum_nMax

-- | Bound on arguments accepted by @cSum@
cSum_mMax, cSum_nMax :: Int
cSum_mMax = 4
cSum_nMax = 16

-- | > encodeCV <y 0 .. y k> == encodeCV <y 0 .. y (k-1)> + cSum k (y k)
--
-- where @c@ is a @k@-combination,
-- that is a sorted list of @k@ nonnegative elements.
--
-- @encodeCV@ is in fact a bijection between increasing lists
-- (of non-negative integers) and integers.
--
-- Restriction: @k < cSum_mMax@, @y k < cSum_nMax@.
encodeCV :: Vector Int -> Coord
encodeCV = U.sum . U.imap cSum

-- | Inverse of 'encodeCV'.
--
-- The length of the resulting list must be supplied as a hint
-- (although it could technically be guessed).
decodeCV :: Int -> Coord -> Vector Int
decodeCV k x = U.create (do
  v <- MU.new k
  let
    decode' (-1) _ _ = return ()
    decode' k z x
      | s <= x    = MU.write v k z >> decode' (k-1) (z-1) (x-s)
      | otherwise =                   decode'  k    (z-1)  x
      where
        s = cSum k z
  decode' (k-1) (cSum_nMax-1) x
  return v)

--

-- | Memoization function. (Currently hidden and unused.)
memoCoord :: MU.Unbox a => Coordinate a -> Coordinate a
memoCoord (Coord r e d) = Coord r e (a U.!)
  where a = U.generate r d

-- | @8! = 40320@
coordCornerPermu :: Coordinate CornerPermu
coordCornerPermu =
  Coord {
    range = 40320,
    encode = \(fromCornerPermu -> p) -> encodeFact numCorners $ U.toList p,
    decode = unsafeCornerPermu . U.fromList . decodeFact numCorners
  }

-- | @12! = 479001600@
--
-- A bit too much to hold in memory.
--
-- Holds just right in a Haskell @Int@ (@maxInt >= 2^29 - 1@).
coordEdgePermu :: Coordinate EdgePermu
coordEdgePermu =
  Coord {
    range = 479001600,
    encode = \(fromEdgePermu -> p) -> encodeFact numEdges $ U.toList p,
    decode = unsafeEdgePermu . U.fromList . decodeFact numEdges
  }

-- | @3^7 = 2187@
coordCornerOrien :: Coordinate CornerOrien
coordCornerOrien =
  Coord {
    range = 2187,
    encode = \(fromCornerOrien -> o) -> encodeBaseV 3 . U.tail $ o,
    -- The first orientation can be deduced from the others in a solvable cube
    decode = decode'
  }
  where
    decode' x = unsafeCornerOrien . U.fromList $ h : t
      where h = (3 - sum t) `mod` 3
            t = decodeBase 3 (numCorners - 1) x

-- | @2^11 = 2048@
coordEdgeOrien :: Coordinate EdgeOrien
coordEdgeOrien =
  Coord {
    range = 2048,
    encode = \(fromEdgeOrien -> o) -> encodeBaseV 2 . U.tail $ o,
    decode = decode'
  }
  where
    decode' x = unsafeEdgeOrien . U.fromList $ h : t
      where h = sum t `mod` 2
            t = decodeBase 2 (numEdges - 1) x

-- | @12C4 = 495@
coordUDSlice :: Coordinate UDSlice
coordUDSlice =
  Coord {
    range = 495,
    encode = \(fromUDSlice -> s) -> encodeCV s,
    decode = unsafeUDSlice . decodeCV numUDSEdges
  }

-- | @4! = 24@
coordUDSlicePermu2 :: Coordinate UDSlicePermu2
coordUDSlicePermu2 =
  Coord {
    range = 24,
    encode = \(fromUDSlicePermu2 -> sp) -> encodeFact numUDSEdges $ U.toList sp,
    decode = unsafeUDSlicePermu2 . U.fromList . decodeFact numUDSEdges
  }

-- | @8! = 40320@
coordUDEdgePermu2 :: Coordinate UDEdgePermu2
coordUDEdgePermu2 =
  Coord {
    range = 40320,
    encode = \(fromUDEdgePermu2 -> e) -> encodeFact numE $ U.toList e,
    decode = unsafeUDEdgePermu2 . U.fromList . decodeFact numE
  }
  where numE = numEdges - numUDSEdges

-- | Checks over the range @range@ that:
--
-- > encode . decode == id
--
checkCoord :: Coordinate a -> Bool
checkCoord (Coord range encode decode)
  = and [ k == encode (decode k) | k <- [0 .. range - 1]]

-- | Endofunctions
type Endo a = a -> a

-- | Lift an endofunction to its coordinate representation,
-- the dictionary provides a @Coord@ encoding.
--
-- That is, we construct a vector @v@ such that, basically,
--
-- > decode (v ! encode x) == f x
--
-- So function application becomes simply vector indexing.
endoVector :: Coordinate a -> Endo a -> Vector Coord
endoVector (Coord range encode decode) f
  = U.generate range $ encode . f . decode

-- | The 'cubeAction' method is partially applied to a 'Cube'
-- and turned into an 'Endo' function.
--
-- The 'CA a' type argument controls the refinement of the endofunction.
cubeActionToEndo :: CubeAction a => Cube -> Endo a
cubeActionToEndo c = (`cubeAction` c)

-- | Composition of 'endoVector' and 'cubeAction'.
--
-- > CubeAction a => Coordinate a -> Cube -> Vector Coord
moveTable :: CubeAction a => Coordinate a -> Cube -> Vector Coord
moveTable coord = endoVector coord . cubeActionToEndo

--moveTable :: CubeAction a => Coordinate a -> Cube -> Vector Coord
--moveTable a cube = U.generate (range a) $ encode a . (`cubeAction` cube) . decode a
--
--moveTablePair :: (CubeAction a, CubeAction b) => Coordinate (a,b) -> Cube -> Vector Coord
--moveTablePair c@(PairCoord a b) cube =
--  va `seq` vb `seq` U.generate (range c) $ \((`divMod` range b) -> (i, j)) ->
--    (va U.! i) * range b + vb U.! j
--  where
--    va = moveTable a cube
--    vb = moveTable b cube

