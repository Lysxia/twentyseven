{- |
   Encoding cube projections as @Int@ coordinates.

   Explicit dictionary passing style:
   using a class would require explicit type annotations /anyway/.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}
module Rubik.Cube.Coord where

import Rubik.Cube.Cubie.Internal
import Rubik.Misc

import Control.Applicative
import Control.Arrow
import Control.Monad.ST

import Data.Binary.Store (Binary)
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import System.Random

-- * Raw coordinates

-- Unwrapped coordinate
type RawCoord' = Int

-- MaxInt 2^29 = 479001600 (at least, according to the standard)
-- | Encoding to an efficient datatype
-- for which it is possible to build tables
-- instead of computing functions.
newtype RawCoord a = RawCoord { unRawCoord :: RawCoord' } deriving (Eq, Ord, Show)

newtype RawVector a b = RawVector { unRawVector :: U.Vector b }

newtype RawMove a = RawMove { unRawMove :: U.Vector RawCoord' }
  deriving (Binary)

(!$) :: RawMove a -> RawCoord a -> RawCoord a
RawMove v !$ RawCoord i = RawCoord (v U.! i)

(!.) :: MU.Unbox b => RawVector a b -> RawCoord a -> b
RawVector v !. RawCoord i = v U.! i

-- * Dictionaries

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
data RawEncoding a = RawEncoding {
    range  :: Int, {- ^ Number of elements that can be converted.
                         Their values are to lie in @[0 .. range c - 1]@. -}
    encode :: a -> RawCoord a,
    decode :: RawCoord a -> a }

-- | Memoization function. (Currently hidden and unused.)
memoCoord :: RawEncoding a -> RawEncoding a
memoCoord (RawEncoding r e d) = RawEncoding r e ((a V.!) . unRawCoord)
  where a = V.generate r (d . RawCoord)

-- ** Instances
-- | The number of elements of every set is given.

-- | @8! = 40320@
rawCornerPermu :: RawEncoding CornerPermu
rawCornerPermu =
  RawEncoding {
    range = 40320,
    encode = RawCoord . encodeFact numCorners . U.toList . fromCornerPermu,
    decode = unsafeCornerPermu . U.fromList . decodeFact numCorners numCorners . unRawCoord
  }

-- | @12! = 479001600@
--
-- A bit too much to hold in memory.
--
-- Holds just right in a Haskell @Int@ (@maxInt >= 2^29 - 1@).
rawEdgePermu :: RawEncoding EdgePermu
rawEdgePermu =
  RawEncoding {
    range = 479001600,
    encode = RawCoord . encodeFact numEdges . U.toList . fromEdgePermu,
    decode = unsafeEdgePermu . U.fromList . decodeFact numEdges numEdges . unRawCoord
  }

-- | @3^7 = 2187@
rawCornerOrien :: RawEncoding CornerOrien
rawCornerOrien =
  RawEncoding {
    range = 2187,
    encode = RawCoord . rangeCheck . encodeBaseV 3 . U.tail . fromCornerOrien,
    -- The first orientation can be deduced from the others in a solvable cube
    decode = decode
  }
  where
    rangeCheck x | x < 2187 = x
    rangeCheck x = error $ show x
    decode (RawCoord x) = unsafeCornerOrien . U.fromList $ h : t
      where h = (3 - sum t) `mod` 3
            t = decodeBase 3 (numCorners - 1) x

-- | @2^11 = 2048@
rawEdgeOrien :: RawEncoding EdgeOrien
rawEdgeOrien =
  RawEncoding {
    range = 2048,
    encode = RawCoord . encodeBaseV 2 . U.tail . fromEdgeOrien,
    decode = decode'
  }
  where
    decode' (RawCoord x) = unsafeEdgeOrien . U.fromList $ h : t
      where h = sum t `mod` 2
            t = decodeBase 2 (numEdges - 1) x

numUDS = numUDSliceEdges

-- | 12! / 8! = 11880
rawUDSlicePermu :: RawEncoding UDSlicePermu
rawUDSlicePermu =
  RawEncoding {
    range = 11880,
    encode = RawCoord . encodeFact numEdges . U.toList . fromUDSlicePermu,
    decode = unsafeUDSlicePermu . U.fromList . decodeFact numEdges numUDS . unRawCoord
  }

-- | @12C4 = 495@
rawUDSlice :: RawEncoding UDSlice
rawUDSlice =
  RawEncoding {
    range = 495,
    encode = RawCoord . encodeCV . fromUDSlice,
    decode = unsafeUDSlice . decodeCV numUDS . unRawCoord
  }

-- | @4! = 24@
rawUDSlicePermu2 :: RawEncoding UDSlicePermu2
rawUDSlicePermu2 =
  RawEncoding {
    range = 24,
    encode = RawCoord . encodeFact numUDS . U.toList . fromUDSlicePermu2,
    decode = unsafeUDSlicePermu2 . U.fromList . decodeFact numUDS numUDS . unRawCoord
  }

-- | @8! = 40320@
rawUDEdgePermu2 :: RawEncoding UDEdgePermu2
rawUDEdgePermu2 =
  RawEncoding {
    range = 40320,
    encode = RawCoord . encodeFact numE . U.toList . fromUDEdgePermu2,
    decode = unsafeUDEdgePermu2 . U.fromList . decodeFact numE numE . unRawCoord
  }
  where numE = numEdges - numUDS

encode2 :: RawEncoding a -> RawEncoding b -> RawEncoding (a, b)
encode2 a b =
  RawEncoding {
    range = range a * range b,
    encode = \(encode a -> RawCoord a_, encode b -> RawCoord b_) ->
              RawCoord (a_ * range b + b_),
    decode = \(RawCoord ab_) ->
              let (a_, b_) = ab_ `divMod` range b
              in (decode a (RawCoord a_), decode b (RawCoord b_))
  }

rawFlipUDSlicePermu = encode2 rawUDSlicePermu rawEdgeOrien 

-- * Table building

-- | Endofunctions
type Endo a = a -> a

-- | Lift an endofunction to its coordinate representation,
-- the dictionary provides a @RawCoord@ encoding.
--
-- That is, we construct a vector @v@ such that, basically,
--
-- > decode (v ! encode x) == f x
--
-- So function application becomes simply vector indexing.
endoVector :: RawEncoding a -> Endo a -> RawMove a
endoVector (RawEncoding range encode decode) f
  = RawMove . U.generate range $ unRawCoord . encode . f . decode . RawCoord

-- | The 'cubeAction' method is partially applied to a 'Cube'
-- and turned into an 'Endo' function.
--
-- The 'CA a' type argument controls the refinement of the endofunction.
cubeActionToEndo :: CubeAction a => Cube -> Endo a
cubeActionToEndo c = (`cubeAction` c)

-- | Composition of 'endoVector' and 'cubeAction'.
moveTable :: CubeAction a => RawEncoding a -> Cube -> RawMove a
moveTable enc = endoVector enc . cubeActionToEndo

symToEndo :: (Cube -> a -> a) -> Cube -> Endo a
symToEndo = id

symTable :: (Cube -> a -> a) -> RawEncoding a -> Cube -> RawMove a
symTable conj enc = endoVector enc . symToEndo conj

-- * Miscellaneous

-- | Checks over the range @range@ that:
--
-- > encode . decode == id
--
checkCoord :: RawEncoding a -> Bool
checkCoord (RawEncoding range encode decode)
  = and [ k == encode (decode k) | k <- fmap RawCoord [0 .. range - 1] ]

randomRaw :: RawEncoding a -> IO (RawCoord a)
randomRaw c = RawCoord <$> randomRIO (0, range c - 1)

-- * Helper
-- | Helper functions to define the dictionaries

-- ** Fixed base

-- | If
-- @all (`elem` [0 .. b-1]) v@
-- then @v@ is the base @b@ representation of
-- @encode b v@
-- such that its least significant digit is @head v@.
--
-- For any @n@, @encodeBase b@ is a bijection from lists of length @n@
-- with elements in @[0 .. b-1]@ to @[0 .. b^n - 1]@
encodeBase :: Int -> [Int] -> Int
encodeBase b = foldr1 (\x y -> x + b * y)

-- | Vector version of 'encodeBase'.
encodeBaseV :: Int -> Vector Int -> Int
encodeBaseV b = U.foldr1' (\x y -> x + b * y)

-- | @len@ is the length of the resulting vector
--
-- > encodeBase b . decodeBase b len == id
-- > decodeBase b len . encodeBase b == id
--
decodeBase :: Int -> Int -> Int -> [Int]
decodeBase b len = take len . unfoldr (\x -> Just (x `modDiv` b))
  where modDiv = ((.).(.)) (\(x,y) -> (y,x)) divMod

-- ** Factorial radix

-- | Input list must be a @k@-permutation of @[0 .. n-1]@.
--
-- @encodeFact@ is a bijection between k-permutations of @[0 .. n-1]@
-- and @[0 .. (fact n / fact (n-k)) - 1]@.
encodeFact :: Int -> [Int] -> Int
encodeFact n [] = 0
encodeFact n (y : ys) = y + n * encodeFact (n - 1) ys'
  where
    ys' = case elemIndex (n - 1) ys of
        Nothing -> ys -- y == n - 1
        Just k -> subs k y ys -- recovers a subpermutation of @[0 .. n-2]@

-- | Inverse of 'encodeFact'.
--
-- > encodeFact n . decodeFact n k == id -- k <= n
-- > decodeFact n k . encodeFact n == id -- on k-permutations
--
decodeFact :: Int -> Int -> Int -> [Int]
decodeFact n 0 _ = []
decodeFact n k x = y : ys
  where
    (q, y) = x `divMod` n
    ys' = decodeFact (n - 1) (k - 1) q
    ys = case elemIndex y ys' of
        Nothing -> ys' -- y == n - 1
        Just k -> subs k (n - 1) ys'

-- ** Binomial enumeration

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
encodeCV :: Vector Int -> Int
encodeCV = U.sum . U.imap cSum

-- | Inverse of 'encodeCV'.
--
-- The length of the resulting list must be supplied as a hint
-- (although it could technically be guessed).
decodeCV :: Int -> Int -> Vector Int
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
