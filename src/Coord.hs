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
  Endo,
  Table,
  EndoCoord,
  endoTable,
  endoLift,

  -- * Miscellaneous

  listArray',
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
  encodeC,
  decodeC,
  ) where

import Misc
import Cubie

import Data.Array.Unboxed
import Data.List

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
-- > inRange (cBound d) $ encode x
--
-- > encode . decode == id
--
-- > decode . encode == id
--
data Coordinate a =
  Coordinate {
    cBound :: (Coord, Coord),
    encode :: a -> Coord,
    decode :: Coord -> a
  }

-- Fixed base representation

-- | If
-- @all (0 <=) l && all (< b) l@
-- then @l@ is the base @b@ representation of
-- @encode b l@
-- such that its least significant digit is @head l@.
encodeBase :: Int -> [Int] -> Coord
encodeBase b = foldr1 (flip $ (+) . (* b))

-- | @len@ is the length of the resulting list
--
-- > encodeBase b . decodeBase b len == id
--
-- > decodeBase b len . encodeBase b == id
--
decodeBase :: Int -> Int -> Coord -> [Int]
decodeBase _ size 0 = replicate size 0
decodeBase b size x = least : more
  where least = x `mod` b
        more = decodeBase b (size - 1) $ x `div` b

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
--
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

-- | @encodeC n c@
--
-- where @c@ is a @k@-combination
encodeC :: Int -> [Int] -> Coord
encodeC _      []  = 0
encodeC n (a : as) = encode' 0 a as
  where encode' k m []       = sum [m' `choose` k | m' <- [m+1..n-1]]
        encode' k m (a : as) = sum [m' `choose` k | m' <- [m+1..a-1]]
                             + encode' (k + 1) a as

-- | @decodeC n k x@
decodeC :: Int -> Int -> Coord -> [Int]
decodeC n k x = decode' n (k - 1) x []
  where decode' n (-1) _ acc = acc
        decode' n k    x acc =
          if x < nCk
            then decode' n' (k - 1) x         (n' : acc)
            else decode' n' k       (x - nCk) acc
          where nCk = n' `choose` k
                n'  = n - 1

--

-- | Specialized instance
listArray' :: Ix i => (i, i) -> [a] -> Array i a
listArray' = listArray

-- | @(0, 8! = 40320)@
coordCornerPermu :: Coordinate CornerPermu
coordCornerPermu =
  Coordinate {
    cBound = (0, 40319),
    encode = \(CornerPermu p) -> encodeFact numCorners p,
    decode = (cpA !)
  }
  where cpA = listArray' (0, 40319) [decode' x | x <- [0..40319]]
        decode' = CornerPermu . decodeFact numCorners

-- | @(0, 12! = 479001600)@
--
-- A bit too much to hold in memory.
--
-- Holds just right in a Haskell @Int@ (@maxInt >= 2^29 - 1@).
coordEdgePermu :: Coordinate EdgePermu
coordEdgePermu =
  Coordinate {
    cBound = (0, 479001599),
    encode = \(EdgePermu p) -> encodeFact numEdges p,
    decode = EdgePermu . decodeFact numEdges
  }

-- | @(0, 3^7 = 2187)@
coordCornerOrien :: Coordinate CornerOrien
coordCornerOrien =
  Coordinate {
    cBound = (0, 2186),
    encode = \(CornerOrien o) -> encodeBase 3 $ tail o,
    -- The first orientation can be deduced from the others in a solvable cube
    decode = (coA !)
  }
  where coA = listArray' (0, 2186) [decode' x | x <- [0..2186]]
        decode' x = CornerOrien $ h : t
          where h = (3 - sum t) `mod` 3
                t = decodeBase 3 (numCorners - 1) x

-- | @(0, 2^11 = 2048)@
coordEdgeOrien :: Coordinate EdgeOrien
coordEdgeOrien =
  Coordinate {
    cBound = (0, 2047),
    encode = \(EdgeOrien o) -> encodeBase 2 $ tail o,
    decode = (eoA !)
  }
  where eoA = listArray' (0, 2047) [decode' x | x <- [0..2047]]
        decode' x = Cubie.EdgeOrien $ h : t
          where h = sum t `mod` 2
                t = decodeBase 2 (numEdges - 1) x

-- | @(0, 12C4 = 495)@
coordUDSlice :: Coordinate UDSlice
coordUDSlice =
  Coordinate {
    cBound = (0, 494),
    encode = \(UDSlice s) -> encodeC numEdges s,
    decode = UDSlice . decodeC numEdges numUDSEdges
  }

-- | @(0, 4! = 24)@
coordUDSlicePermu :: Coordinate UDSlicePermu
coordUDSlicePermu =
  Coordinate {
    cBound = (0, 23),
    encode = \(UDSlicePermu sp) -> encodeFact numUDSEdges sp,
    decode = UDSlicePermu . decodeFact numUDSEdges
  }

-- | @(0, 8! = 40320)@
coordUDEdgePermu :: Coordinate UDEdgePermu
coordUDEdgePermu =
  Coordinate {
    cBound = (0, 40319),
    encode = \(UDEdgePermu e) -> encodeFact numE e,
    decode = UDEdgePermu . decodeFact numE
  }
  where numE = numEdges - numUDSEdges

--

-- | Checks over the range @cBound@ that:
--
-- > encode . decode == id
--
checkCoord :: Coordinate a -> Bool
checkCoord coord = and [k == encode coord (decode coord k) | k <- [i..j]]
  where (i, j) = cBound coord

--

-- | Endofunctions
type Endo a = a -> a

-- | Tables of @Endo Coord@ functions
type Table = UArray Coord Coord

-- | Functional wrapping of @MoveTable@
data EndoCoord = EndoCoord
  { ecBounds :: (Coord, Coord)
  , endo :: Endo Coord }

-- | Table of an endofunction on a finite domain in @Coord@
endoTable :: (Coord, Coord) -> Endo Coord -> Table
endoTable (i, j) endo = listArray (i, j) l
  where l = [endo x | x <- [i .. j]]

-- | Lift an endofunction to its coordinate representation,
-- the dictionary provides a @Coord@ encoding.
endoLift :: Coordinate a -> Endo a -> EndoCoord
endoLift coord endo = EndoCoord (cBound coord) (mt !)
  where mt = endoTable (cBound coord) $ encode coord . endo . decode coord
