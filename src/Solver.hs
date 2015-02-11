{-# LANGUAGE ViewPatterns#-}
module Solver (
  DInt,

  encodeCI',
  extract,
  flatIndex,
  searchWith,

  -- * Move tables
  CoordInfo (..),

  -- ** 18 moves
  move18CornerPermu,
  move18CornerOrien,

  move18EdgeOrien,

  move18UDSlicePermu,
  move18LRSlicePermu,
  move18FBSlicePermu,

  move18UDSlice,
  move18LRSlice,
  move18FBSlice,

  -- ** 10 moves (G1 group)
  move18to10,
  move10CornerPermu,
  move10UDSlicePermu2,
  move10UDEdgePermu2,
  
  -- * Pruning tables
  DistIndexType (..),

  dist_CornerOrien_UDSlice,
  dist_CornerOrien_LRSlice,
  dist_CornerOrien_FBSlice,

  dist_UDSlicePermu_EdgeOrien,

  dist_EdgeOrien_UDSlice,
  dist_EdgeOrien_LRSlice,
  dist_EdgeOrien_FBSlice,

  dist_CornerPermu,
  dist_CornerOrien,
  dist_EdgeOrien,

  -- ** Two-phase algorithm: phase 2
  dist_EdgePermu2,
  dist_CornerPermu_UDSlicePermu2,

  -- ** Builders
  distanceWithCI,
  distanceWithCI2,
  distanceWith2,
  ) where

import Prelude hiding ( maximum )

import Coord
import Cubie
import Distances
import IDA
import Moves
import Misc
import Symmetry

import Control.Applicative

import Data.Foldable ( Foldable, maximum, toList )
import Data.Int ( Int8 )
import Data.Monoid
import Data.StrictTuple
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | Distances only go up to 20 for 3x3 Rubik's cubes.
type DInt = Int8

encodeCI' :: Applicative f => f CoordInfo -> Cube -> (Int, f Int)
encodeCI' ci = (,) 6 . (<$> ci) . flip encodeCI

extract :: Result DInt ElemMove -> Move
extract = head . snd . head

-- | Convert 2D indices to 1D.
--
-- > \n x y -> x * n + y
flatIndex :: Int -> Int -> Int -> Int
flatIndex n x y = x * n + y

-- | ==Branching reduction
--
-- The @Int@ projection keeps track of the latest move (@== 6@
-- for the starting point).
--
-- We can indeed reduce the branching factor from 18 to 15
-- by considering that successive moves on the same face
-- can and will be shortened as a single move.
--
-- Furthermore, since moves on opposite faces commute, we may force
-- them to be in an arbitrary order, reducing the branching factor
-- to 12 after half of the moves (U, L, F).
--
searchWith
  :: (Applicative f, Foldable f, Eq (f Coord))
  => [ElemMove]
  -> f CoordInfo
  -> (f [Vector Coord] -> [f (Vector Coord)])
  -> [DistParam]
  -> Search DInt ElemMove (Int, f Int)
{-# INLINE searchWith #-}
searchWith moveNames ci transpose dists = Search goal estm edges
  where
    goal = let g1 = encodeCI <$> ci <*> pure iden in (g1 ==) . snd
    estm (_, t) = maximum $ estm' <$> dists <*> pure t
    estm' (d, One x) = \t -> d U.! (t ? x)
    estm' (d, Two dim (x, y)) = \t -> d U.! flatIndex dim (t ? x) (t ? y)
    edges (i, t)
      = [ x `seq` Succ l 1 (fromEnum j, x)
        | (l@(_, j), succs) <- succVector V.! i, let x = (U.!) <$> succs <*> t]
    -- For every move, filter out "larger" moves for an arbitrary total order
    succVector
      = V.snoc
          (V.generate 6 $ \(toEnum -> i) ->
            [ m | m@((_, j), _) <- moves,
              not (i == j || oppositeAndGT j i) ])
          moves
    moves = zip moveNames . transpose $ movesCI <$> ci
    (?) = (!!) . toList -- Index in a Foldable

-- * Move tables
data CoordInfo = CoordInfo {
    movesCI :: [Vector Coord],
    encodeCI :: Cube -> Coord
  }

table :: CubeAction a => [Cube] -> Coordinate a -> [Vector Coord]
table moves coord = moveTable coord <$> moves

coordInfo :: (CubeAction a, FromCube a) => [Cube] -> Coordinate a -> CoordInfo
coordInfo moves coord = CoordInfo (table moves coord) (encode coord . fromCube)

move18CornerOrien = coordInfo move18 coordCornerOrien
move18CornerPermu = coordInfo move18 coordCornerPermu

move18EdgeOrien = coordInfo move18 coordEdgeOrien

move18UDSlicePermu = coordInfo move18 coordUDSlicePermu
move18LRSlicePermu = symmetricCI move18UDSlicePermu surf3
move18FBSlicePermu = symmetricCI move18UDSlicePermu (surf3 <>^ 2)

move18UDSlice = coordInfo move18 coordUDSlice
move18LRSlice = symmetricCI move18UDSlice surf3
move18FBSlice = symmetricCI move18UDSlice (surf3 <>^ 2)

symmetricCI (CoordInfo m e) sym = CoordInfo m (e . (sym <>))

move18to10 = [ n - 1 + 3 * fromEnum m | (n, m) <- move10Names ]

move10CornerPermu = move18CornerPermu {
    movesCI = composeList (movesCI move18CornerPermu) move18to10 }
move10UDSlicePermu2 = coordInfo move10 coordUDSlicePermu2
move10UDEdgePermu2 = coordInfo move10 coordUDEdgePermu2

-- * Pruning tables
type DistParam = (Vector DInt, DistIndexType)

data DistIndexType
  = One { pos1 :: Int }
  | Two { dim2 :: Int, pos2 :: (Int, Int) }

dist_CornerOrien_UDSlice = distanceWithCI2 move18CornerOrien move18UDSlice
dist_CornerOrien_LRSlice = distanceWithCI2 move18CornerOrien move18LRSlice
dist_CornerOrien_FBSlice = distanceWithCI2 move18CornerOrien move18FBSlice

dist_UDSlicePermu_EdgeOrien = distanceWithCI2 move18UDSlicePermu move18EdgeOrien

dist_EdgeOrien_UDSlice = distanceWithCI2 move18EdgeOrien move18UDSlice
dist_EdgeOrien_LRSlice = distanceWithCI2 move18EdgeOrien move18LRSlice
dist_EdgeOrien_FBSlice = distanceWithCI2 move18EdgeOrien move18FBSlice

dist_CornerPermu = distanceWithCI move18CornerPermu
dist_CornerOrien = distanceWithCI move18CornerOrien
dist_EdgeOrien = distanceWithCI move18EdgeOrien

dist_EdgePermu2 = distanceWithCI2 move10UDEdgePermu2 move10UDSlicePermu2

dist_CornerPermu_UDSlicePermu2
  = distanceWithCI2 move10CornerPermu move10UDSlicePermu2

--

distanceWithCI :: CoordInfo -> Vector DInt
distanceWithCI (CoordInfo vs encode) = distances n root neighbors
  where
    n = U.length (head vs)
    root = encode iden
    neighbors = liftA2 (U.!) vs . pure

distanceWith2
  :: Coord -> [Vector Coord] -> Coord -> [Vector Coord] -> Vector DInt
distanceWith2 root1 vs1 root2 vs2
  = distances n root neighbors
  where
    n = U.length (head vs1) * n2
    n2 = U.length (head vs2)
    root = flatIndex n2 root1 root2
    neighbors ((`divMod` n2) -> (x1, x2))
      = zipWith (flatIndex n2)
          ((U.! x1) <$> vs1)
          ((U.! x2) <$> vs2)

distanceWithCI2 :: CoordInfo -> CoordInfo -> Vector DInt
distanceWithCI2 (CoordInfo moves1 encode1) (CoordInfo moves2 encode2)
  = distanceWith2 (encode1 iden) moves1 (encode2 iden) moves2

