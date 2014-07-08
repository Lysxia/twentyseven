{- |
   Encoding cube projections as @Int@ coordinates.

   Explicit dictionary passing style:
   using a class would require explicit type annotations /anyway/.
-}

module Coord (
  -- * Dictionaries

  Coord,
  Coordinate (..),

  -- ** Instances
  -- | Bounds are given

  coordCornerPermu,
  coordCornerOrien,
  coordEdgePermu,
  coordEdgeOrien,

  coordUDSlice,
  coordUDSlicePermu,
  coordUDEdgePermu,

  -- * Table building
  Table,
  endoTable,
  endoLift,

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
-- Synonymous with instances for both
-- @(Enum a, Bounded a)@.
--
-- > inRange (cMax d) $ encode x
-- > encode . decode == id
-- > decode . encode == id
--
data Coordinate a =
  Coordinate {
    cMax :: Coord,
    encode :: a -> Coord,
    decode :: Coord -> a
  }

-- Fixed base representation

-- | If
-- @all (0 <=) v && all (< b) v@
-- then @v@ is the base @b@ representation of
-- @encode b v@
-- such that its least significant digit is @head v@.
encodeBase :: Int -> [Int] -> Coord
encodeBase b = foldr1 (\x y -> x + b * y)

encodeBaseV :: Int -> Vector Int -> Coord
encodeBaseV b = U.foldr1' (\x y -> x + b * y)

-- | @len@ is the length of the resulting vector
--
-- > encodeBase b . decodeBase b len == id
-- > decodeBase b len . encodeBase b == id
--
decodeBase :: Int -> Int -> Coord -> [Int]
decodeBase b len = take len . unfoldr (\x -> Just (x `mod` b, x `div` b))

-- Factorial radix representation

-- | Input list must be a permutation of @[0 .. n - 1]@
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

-- |
--
-- > encodeFact n . decodeFact n == id
-- > decodeFact n . encodeFact n == id
--
decodeFact :: Int -> Coord -> [Int]
decodeFact 0 _ = []
decodeFact 1 _ = [0]
decodeFact n x = insert' k (n - 1) l
  where l = decodeFact (n - 1) (x `div` n)
        k = (x `mod` n)

-- $binom
-- Bijection between @[0 .. choose n (k - 1)]@
-- and @k@-combinations of @[0 .. n - 1]@,
-- see <http://kociemba.org/math/UDSliceCoord.htm>

-- | @encodeCV n c@
--
-- where @c@ is a @k@-combination,
-- that is a sorted list of @k@ elements in @[0..n-1]@.
encodeCV :: Int -> Vector Int -> Coord
encodeCV n v = U.sum s + chooseSum (U.length v - 1) (U.last v) n
  where
    chooseSum k m a = sum . map (`choose` k) $ [m+1 .. a-1]
    s = U.izipWith chooseSum v (U.tail v)

-- | @decodeCV n k x@
decodeCV :: Int -> Int -> Coord -> Vector Int
decodeCV n k x = runST (do
  v <- MU.new k
  let
    decode' _ (-1) _ = return ()
    decode' n k    x
      | x < nCk = MU.write v k n' >> decode' n' (k - 1) x
      | otherwise =                  decode' n' k (x - nCk)
      where
        nCk = n' `choose` k
        n' = n - 1
  decode' n (k - 1) x
  U.unsafeFreeze v)

--

memoCoord :: MU.Unbox a => Coordinate a -> Coordinate a
memoCoord c =
  c { decode = (a U.!) }
  where a = U.generate (cMax c) (decode c)

-- | @8! = 40320@
coordCornerPermu :: Coordinate CornerPermu
coordCornerPermu =
  Coordinate {
    cMax = 40319,
    encode = \(CornerPermu p) -> encodeFact numCorners $ U.toList p,
    decode = CornerPermu . U.fromList . decodeFact numCorners
  }

-- | @12! = 479001600@
--
-- A bit too much to hold in memory.
--
-- Holds just right in a Haskell @Int@ (@maxInt >= 2^29 - 1@).
coordEdgePermu :: Coordinate EdgePermu
coordEdgePermu =
  Coordinate {
    cMax = 479001599,
    encode = \(EdgePermu p) -> encodeFact numEdges $ U.toList p,
    decode = EdgePermu . U.fromList . decodeFact numEdges
  }

-- | @3^7 = 2187@
coordCornerOrien :: Coordinate CornerOrien
coordCornerOrien =
  Coordinate {
    cMax = 2186,
    encode = \(CornerOrien o) -> encodeBaseV 3 . U.tail $ o,
    -- The first orientation can be deduced from the others in a solvable cube
    decode = decode'
  }
  where
    decode' x = CornerOrien . U.fromList $ h : t
      where h = (3 - sum t) `mod` 3
            t = decodeBase 3 (numCorners - 1) x

-- | @2^11 = 2048@
coordEdgeOrien :: Coordinate EdgeOrien
coordEdgeOrien =
  Coordinate {
    cMax = 2047,
    encode = \(EdgeOrien o) -> encodeBaseV 2 . U.tail $ o,
    decode = decode'
  }
  where
    decode' x = Cubie.EdgeOrien . U.fromList $ h : t
      where h = sum t `mod` 2
            t = decodeBase 2 (numEdges - 1) x

-- | @12C4 = 495@
coordUDSlice :: Coordinate UDSlice
coordUDSlice =
  Coordinate {
    cMax = 494,
    encode = \(UDSlice s) -> encodeCV numEdges s,
    decode = UDSlice . decodeCV numEdges numUDSEdges
  }

-- | @4! = 24@
coordUDSlicePermu :: Coordinate UDSlicePermu
coordUDSlicePermu =
  Coordinate {
    cMax = 23,
    encode = \(UDSlicePermu sp) -> encodeFact numUDSEdges $ U.toList sp,
    decode = UDSlicePermu . U.fromList . decodeFact numUDSEdges
  }

-- | @8! = 40320@
coordUDEdgePermu :: Coordinate UDEdgePermu
coordUDEdgePermu =
  Coordinate {
    cMax = 40319,
    encode = \(UDEdgePermu e) -> encodeFact numE $ U.toList e,
    decode = UDEdgePermu . U.fromList . decodeFact numE
  }
  where numE = numEdges - numUDSEdges

-- | @495 * 2048 = 1013760@
coordFlipUDSlice :: Coordinate (EdgeOrien, UDSlice)
coordFlipUDSlice =
  Coordinate {
    cMax = 1013759,
    encode = encode',
    decode = decode'
  }
  where
    encode' (eo, s) = encode coordEdgeOrien eo + 2048 * encode coordUDSlice s
    decode' = decode coordEdgeOrien . (`mod` 2048)
          &&& decode coordUDSlice . (`div` 2048)

--

-- | Checks over the range @cMax@ that:
--
-- > encode . decode == id
--
checkCoord :: Coordinate a -> Bool
checkCoord coord = and [k == encode coord (decode coord k) | k <- [0 .. n]]
  where n = cMax coord

--

-- | Tables of @Coord -> Coord@ functions
type Table = Vector Coord

-- | Table of an endofunction on a finite domain in @Coord@
endoTable :: Coord -> (Coord -> Coord) -> Table
endoTable = U.generate

-- | Lift an endofunction to its coordinate representation,
-- the dictionary provides a @Coord@ encoding.
endoLift :: Coordinate a -> (a -> a) -> (Coord -> Coord)
endoLift coord endo = (mt U.!)
  where mt = endoTable (cMax coord) $ encode coord . endo . decode coord

