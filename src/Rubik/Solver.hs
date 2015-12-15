{-# LANGUAGE ViewPatterns #-}
module Rubik.Solver where

import Prelude hiding ( maximum )

import Rubik.Cube
import Rubik.Distances
import Rubik.IDA
import Rubik.Misc
import Rubik.Solver.Template
import Rubik.Symmetry

import Control.Applicative

import Data.Binary.Store
import Data.Foldable ( Foldable, maximum, foldr', toList )
import Data.Int ( Int8 )
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.StrictTuple
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

type MaybeFace = Int

data Projection as a = Projection
  { convertP :: (Cube -> a)
  , cube0 :: a
  , indexP :: (as -> a -> a)
  }

type Projection' a = Projection (RawMove a) (RawCoord a)

newtype Distance m a = Distance { distanceP :: a -> DInt }

infixr 4 |:|, |.|

{-# INLINE (|:|) #-}
(|:|) :: (TupleCons as bs cs, TupleCons a b c)
  => Projection as a -> Projection bs b -> Projection cs c
a |:| b = Projection
  { convertP = liftA2 (|*|) (convertP a) (convertP b)
  , cube0 = cube0 a |*| cube0 b
  , indexP = \(split -> (as_, bs_)) (split -> (a_, b_)) -> indexP a as_ a_ |*| indexP b bs_ b_ }

{-# INLINE (|.|) #-}
(|.|) :: Projection as a -> Projection bs b -> Projection (Tuple2 as bs) (Tuple2 a b)
a |.| b = a |:| b'
  where
    b' = Projection
      { convertP = Tuple1 . convertP b
      , cube0 = Tuple1 (cube0 b)
      , indexP = \(Tuple1 bs_) (Tuple1 b_) -> Tuple1 (indexP b bs_ b_) }

{-# INLINE contramapDistance #-}
contramapDistance :: (b -> a) -> Distance m a -> Distance m b
contramapDistance f a = Distance
  { distanceP = distanceP a . f }

{-# INLINE (>$<) #-}
(>$<) = contramapDistance

{-# INLINE maxDistance #-}
maxDistance :: Foldable f => f (Distance m a) -> Distance m a
maxDistance as = Distance {
  distanceP = \a_ -> foldr' (max . \a -> distanceP a a_) 0 as }

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
{-# INLINE mkSearch #-}
mkSearch
  :: (Eq a)
  => MoveTag m [ElemMove] -> MoveTag m [as] -> Projection as a -> Distance m a
  -> Search [] DInt ElemMove (Tag a)
mkSearch (MoveTag moveNames) (MoveTag ms) ps pd = Search
  { goal = (== cube0 ps) . snd
  , estm = distanceP pd . snd
  , edges = \(i, t) -> fmap
              (\(l, succs, j') ->
                let x = indexP ps succs t in x `seq` Succ l 1 (j', x))
              (succVector V.! i) }
  where
    -- For every move, filter out "larger" moves for an arbitrary total order
    succVector
      = V.generate 7 $ \i' ->
          [ m' | m'@(l@(_, j), _, _) <- moves,
            i' == 6 || (let i = toEnum i' in not (i == j || oppositeAndGT j i)) ]
    moves = [ (l,m,fromEnum j) | (l@(_, j), m) <- zip moveNames ms ]

-- | Distances only go up to 20 for 3x3 Rubik's cubes.
type DInt = Int8

type Tag a = (Int, a)

-- * Move tables

--{-# INLINE storedRawMove #-}
--storedRawMove
--  :: (CubeAction a, FromCube a)
--  => String -> MoveTag m [Cube] -> RawEncoding a
--  -> (Store (MoveTag m [RawMove a]), Preload (Projection' a))
--storedRawMove name moves enc =
--  let x = storedRawMoveTables name moves enc
--      y = storedRawProjection x enc
--  in (x, y)

{-# INLINE rawMoveTables #-}
rawMoveTables :: CubeAction a => MoveTag m [Cube] -> RawEncoding a -> MoveTag m [RawMove a]
rawMoveTables (MoveTag moves) enc = MoveTag $ moveTable enc <$> moves

{-# INLINE storedRawMoveTables #-}
storedRawMoveTables :: CubeAction a => String -> MoveTag m [Cube] -> RawEncoding a
  -> Store (MoveTag m [RawMove a])
storedRawMoveTables name moves enc = store name (rawMoveTables moves enc)

{-# INLINE rawProjection #-}
rawProjection :: FromCube a => RawEncoding a -> Projection' a
rawProjection enc = Projection
  { convertP = convert
  , cube0 = convert iden
  , indexP = (!$) }
  where
    convert = encode enc . fromCube

{-
{-# INLINE symmetricProj #-}
symmetricProj :: FromCube a => Store (MoveTag m [RawMove a]) -> RawEncoding a
  -> Symmetry sym
  -> Preload (Projection' (Symmetric sym a))
symmetricProj store enc sym = mkProjection <$> loadS store
  where
    mkProjection (MoveTag moves) = Projection
      { convertP = convert
      , cube0 = convert iden
      , edgesP = rawMoveSym sym moves
      , indexP = (!$) }
    convert = rawCast . encode enc . fromCube . conjugate (inverse (symAsCube sym))
-}

-- ** 18 moves

{-# INLINE projCornerOrien #-}
move18CornerOrien
  = storedRawMoveTables "move18CornerOrien" move18 rawCornerOrien
projCornerOrien
  = rawProjection rawCornerOrien

move18CornerPermu
  = storedRawMoveTables "move18CornerPermu" move18 rawCornerPermu
{-# INLINE projCornerPermu #-}
projCornerPermu
  = rawProjection rawCornerPermu

{-# INLINE projEdgeOrien #-}
move18EdgeOrien
  = storedRawMoveTables "move18EdgeOrien" move18 rawEdgeOrien
projEdgeOrien = rawProjection rawEdgeOrien

{-# INLINE projUDSlicePermu #-}
move18UDSlicePermu
  = storedRawMoveTables "move18UDSlicePermu" move18 rawUDSlicePermu
projUDSlicePermu = rawProjection rawUDSlicePermu

{-
projLRSlicePermu = symmetricProj move18UDSlicePermu rawUDSlicePermu symmetry_urf3
projFBSlicePermu = symmetricProj move18UDSlicePermu rawUDSlicePermu symmetry_urf3'
-}

move18UDSlice = storedRawMoveTables "move18UDSlice" move18 rawUDSlice
projUDSlice = rawProjection rawUDSlice

{-
projLRSlice = symmetricProj move18UDSlice rawUDSlice symmetry_urf3
projFBSlice = symmetricProj move18UDSlice rawUDSlice symmetry_urf3'
-}

{-# INLINE move18to10 #-}
move18to10 :: MoveTag Move18 [as] -> MoveTag Move10 [as]
move18to10 (MoveTag as) = MoveTag
  (composeList as [ n - 1 + 3 * fromEnum m | (n, m) <- unMoveTag move10Names ])

-- ** 10 moves (G1 group)

move10CornerPermu = store "move10CornerPermu" $ move18to10 (value move18CornerPermu)
move10UDSlicePermu2
  = storedRawMoveTables "move10UDSlicePermu2" move10 rawUDSlicePermu2
{-# INLINE projUDSlicePermu2 #-}
projUDSlicePermu2 = rawProjection rawUDSlicePermu2

move10UDEdgePermu2
  = storedRawMoveTables "move10UDEdgePermu2" move10 rawUDEdgePermu2
{-# INLINE projUDEdgePermu2 #-}
projUDEdgePermu2 = rawProjection rawUDEdgePermu2

-- * Distance tables

{-# INLINE distanceWith2 #-}
distanceWith2
  :: String -> Store (MoveTag m [RawMove a]) -> Store (MoveTag m [RawMove b])
  -> Projection' a -> Projection' b -> RawEncoding a -> RawEncoding b
  -> (Store (Vector DInt), Preload (Distance m (RawCoord a, RawCoord b)))
distanceWith2 name (value -> m1) (value -> m2) proj1 proj2 a b = (s, d <$> loadS s)
  where
    s = store name (distanceWith2' m1 m2 proj1 proj2 n1 n2)
    d dv = Distance $ \(RawCoord a, RawCoord b) -> dv U.! flatIndex n2 a b
    n1 = range a
    n2 = range b

{-# INLINE distanceWith2' #-}
distanceWith2'
  :: MoveTag m [RawMove a] -> MoveTag m [RawMove b]
  -> Projection' a -> Projection' b -> Int -> Int -> Vector DInt
distanceWith2' (MoveTag m1) (MoveTag m2) proj1 proj2 n1 n2 = distances n root neighbors
  where
    n = n1 * n2
    root = flatIndex n2 (unRawCoord (cube0 proj1)) (unRawCoord (cube0 proj2))
    neighbors ((`divMod` n2) -> (x1, x2))
      = zipWith (\v1 v2 -> flatIndex n2
          (unRawCoord . indexP proj1 v1 $ RawCoord x1)
          (unRawCoord . indexP proj2 v2 $ RawCoord x2)) m1 m2

castDistance :: Distance m (RawCoord a) -> Distance m (RawCoord (Symmetric sym a))
castDistance (Distance d) = Distance $ \(RawCoord a) -> d (RawCoord a)

{-# INLINE dist_CornerOrien_UDSlice #-}
(d_CornerOrien_UDSlice, dist_CornerOrien_UDSlice)
  = distanceWith2 "dist_CornerOrien_UDSlice" move18CornerOrien move18UDSlice projCornerOrien projUDSlice rawCornerOrien rawUDSlice

{-
(d_CornerOrien_LRSlice, dist_CornerOrien_LRSlice)
  = distanceWith2 "dist_CornerOrien_LRSlice" projCornerOrien projLRSlice
(d_CornerOrien_FBSlice, dist_CornerOrien_FBSlice)
  = distanceWith2 "dist_CornerOrien_FBSlice" projCornerOrien projFBSlice

(d_UDSlicePermu_EdgeOrien, dist_UDSlicePermu_EdgeOrien)
  = distanceWith2 "dist_UDSlicePermu_EdgeOrien" move18UDSlicePermu move18EdgeOrien projUDSlicePermu projEdgeOrien rawUDSlicePermu rawEdgeOrien
-}

(d_EdgeOrien_UDSlice, dist_EdgeOrien_UDSlice)
  = distanceWith2 "dist_EdgeOrien_UDSlice" move18EdgeOrien move18UDSlice projEdgeOrien projUDSlice rawEdgeOrien rawUDSlice

{-
(d_EdgeOrien_LRSlice, dist_EdgeOrien_LRSlice)
  = distanceWith2 "dist_EdgeOrien_LRSlice" projEdgeOrien projLRSlice
(d_EdgeOrien_FBSlice, dist_EdgeOrien_FBSlice)
  = distanceWith2 "dist_EdgeOrien_FBSlice" projEdgeOrien projFBSlice
-}

{-# INLINE dist_UDEdgePermu2_UDSlicePermu2 #-}
(d_UDEdgePermu2_UDSlicePermu2, dist_UDEdgePermu2_UDSlicePermu2)
  = distanceWith2 "dist_EdgePermu2" move10UDEdgePermu2 move10UDSlicePermu2 projUDEdgePermu2 projUDSlicePermu2 rawUDEdgePermu2 rawUDSlicePermu2

{-# INLINE dist_CornerPermu_UDSlicePermu2 #-}
(d_CornerPermu_UDSlicePermu2, dist_CornerPermu_UDSlicePermu2)
  = distanceWith2 "dist_CornerPermu_UDSlicePermu2" move10CornerPermu move10UDSlicePermu2 projCornerPermu projUDSlicePermu2 rawCornerPermu rawUDSlicePermu2

{-
dist_CornerPermu = distanceWithCI move18CornerPermu
dist_CornerOrien = distanceWithCI move18CornerOrien
dist_EdgeOrien = distanceWithCI move18EdgeOrien
-}
