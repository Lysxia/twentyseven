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

data Projection m as a = Projection
  { convertP :: (Cube -> a)
  , cube0 :: a
  , edgesP :: [as]
  , indexP :: (as -> a -> a)
  }

type Projection' m a = Projection m (RawMove a) (RawCoord a)

newtype Distance m a = Distance { distanceP :: a -> DInt }

infixr 4 |:|, |.|, |:|., |.|.

{-# INLINE (|:|) #-}
(|:|) :: (TupleCons as bs cs, TupleCons a b c)
  => Projection m as a -> Projection m bs b -> Projection m cs c
a |:| b = Projection
  { convertP = liftA2 (|*|) (convertP a) (convertP b)
  , cube0 = cube0 a |*| cube0 b
  , edgesP = zipWith (|*|) (edgesP a) (edgesP b)
  , indexP = \(split -> (as_, bs_)) (split -> (a_, b_)) -> indexP a as_ a_ |*| indexP b bs_ b_ }

{-# INLINE (|.|) #-}
(|.|) :: Projection m as a -> Projection m bs b -> Projection m (Tuple2 as bs) (Tuple2 a b)
a |.| b = a |:| b'
  where
    b' = Projection
      { convertP = Tuple1 . convertP b
      , cube0 = Tuple1 (cube0 b)
      , edgesP = fmap Tuple1 (edgesP b)
      , indexP = \(Tuple1 bs_) (Tuple1 b_) -> Tuple1 (indexP b bs_ b_) }

{-# INLINE (|:|.) #-}
(|:|.) x = liftA2 (|:|) x
{-# INLINE (|.|.) #-}
(|.|.) x = liftA2 (|.|) x

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
  => MoveTag m [ElemMove] -> Projection m as a -> Distance m a
  -> Search [] DInt ElemMove (Tag a)
mkSearch (MoveTag moveNames) ps pd = Search
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
          [ (l,m,j') | (l@(_, j), m) <- moves, let j' = fromEnum j,
            i' == 6 || (let i = toEnum i' in not (i == j || oppositeAndGT j i)) ]
    moves = zip moveNames (edgesP ps)

-- | Distances only go up to 20 for 3x3 Rubik's cubes.
type DInt = Int8

type Tag a = (Int, a)

-- * Move tables

{-# INLINE storedRawMove #-}
storedRawMove
  :: (CubeAction a, FromCube a)
  => String -> MoveTag m [Cube] -> RawEncoding a
  -> (Store (MoveTag m [RawMove a]), Preload (Projection' m a))
storedRawMove name moves enc =
  let x = storedRawMoveTables name moves enc
      y = storedRawProjection x enc
  in (x, y)

{-# INLINE rawMoveTables #-}
rawMoveTables :: CubeAction a => MoveTag m [Cube] -> RawEncoding a -> MoveTag m [RawMove a]
rawMoveTables (MoveTag moves) enc = MoveTag $ moveTable enc <$> moves

{-# INLINE storedRawMoveTables #-}
storedRawMoveTables :: CubeAction a => String -> MoveTag m [Cube] -> RawEncoding a
  -> Store (MoveTag m [RawMove a])
storedRawMoveTables name moves enc = store name (rawMoveTables moves enc)

{-# INLINE storedRawProjection #-}
storedRawProjection :: FromCube a => Store (MoveTag m [RawMove a]) -> RawEncoding a
  -> Preload (Projection' m a)
storedRawProjection store enc = mkProjection <$> loadS store
  where
    mkProjection (MoveTag moves) = Projection
      { convertP = convert
      , cube0 = convert iden
      , edgesP = moves
      , indexP = (!$) }
    convert = encode enc . fromCube

{-# INLINE symmetricProj #-}
symmetricProj :: FromCube a => Store (MoveTag m [RawMove a]) -> RawEncoding a
  -> Symmetry sym
  -> Preload (Projection' m (Symmetric sym a))
symmetricProj store enc sym = mkProjection <$> loadS store
  where
    mkProjection (MoveTag moves) = Projection
      { convertP = convert
      , cube0 = convert iden
      , edgesP = rawMoveSym sym moves
      , indexP = (!$) }
    convert = rawCast . encode enc . fromCube . conjugate (inverse (symAsCube sym))

-- ** 18 moves

{-# INLINE proj18CornerOrien #-}
(move18CornerOrien, proj18CornerOrien)
  = storedRawMove "move18CornerOrien" move18 rawCornerOrien

move18CornerPermu
  = storedRawMoveTables "move18CornerPermu" move18 rawCornerPermu
{-# INLINE proj18CornerPermu #-}
proj18CornerPermu
  = storedRawProjection move18CornerPermu rawCornerPermu

{-# INLINE proj18EdgeOrien #-}
(move18EdgeOrien, proj18EdgeOrien)
  = storedRawMove "move18EdgeOrien" move18 rawEdgeOrien

{-# INLINE proj18UDSlicePermu #-}
(move18UDSlicePermu, proj18UDSlicePermu)
  = storedRawMove "move18UDSlicePermu" move18 rawUDSlicePermu

proj18LRSlicePermu = symmetricProj move18UDSlicePermu rawUDSlicePermu symmetry_urf3
proj18FBSlicePermu = symmetricProj move18UDSlicePermu rawUDSlicePermu symmetry_urf3'

(move18UDSlice, proj18UDSlice) = storedRawMove "move18UDSlice" move18 rawUDSlice
proj18LRSlice = symmetricProj move18UDSlice rawUDSlice symmetry_urf3
proj18FBSlice = symmetricProj move18UDSlice rawUDSlice symmetry_urf3'

{-# INLINE move18to10 #-}
move18to10 :: Projection Move18 as a -> Projection Move10 as a
move18to10 p = p
  { edgesP = composeList (edgesP p) [ n - 1 + 3 * fromEnum m | (n, m) <- unMoveTag move10Names ] }

-- ** 10 moves (G1 group)

{-# INLINE proj10CornerPermu #-}
proj10CornerPermu = move18to10 <$> proj18CornerPermu
move10UDSlicePermu2
  = storedRawMoveTables "move10UDSlicePermu2" move10 rawUDSlicePermu2
{-# INLINE proj10UDSlicePermu2 #-}
proj10UDSlicePermu2 = storedRawProjection move10UDSlicePermu2 rawUDSlicePermu2

move10UDEdgePermu2
  = storedRawMoveTables "move10UDEdgePermu2" move10 rawUDEdgePermu2
{-# INLINE proj10UDEdgePermu2 #-}
proj10UDEdgePermu2 = storedRawProjection move10UDEdgePermu2 rawUDEdgePermu2

-- * Distance tables

{-# INLINE distanceWith2 #-}
distanceWith2
  :: String -> Preload (Projection' m a) -> Preload (Projection' m b) -> RawEncoding a -> RawEncoding b
  -> (Store (Vector DInt), Preload (Distance m (RawCoord a, RawCoord b)))
distanceWith2 name (unwrapPL -> proj1) (unwrapPL -> proj2) a b = (s, d <$> loadS s)
  where
    s = store name (distanceWith2' proj1 proj2 n1 n2)
    d dv = Distance $ \(RawCoord a, RawCoord b) -> dv U.! flatIndex n2 a b
    n1 = range a
    n2 = range b

{-# INLINE distanceWith2' #-}
distanceWith2' :: Projection' m a -> Projection' m b -> Int -> Int -> Vector DInt
distanceWith2' proj1 proj2 n1 n2 = distances n root neighbors
  where
    n = n1 * n2
    root = flatIndex n2 (unRawCoord (cube0 proj1)) (unRawCoord (cube0 proj2))
    neighbors ((`divMod` n2) -> (x1, x2))
      = zipWith (\v1 v2 -> flatIndex n2
          (unRawCoord . indexP proj1 v1 $ RawCoord x1)
          (unRawCoord . indexP proj2 v2 $ RawCoord x2)) (edgesP proj1) (edgesP proj2)

castDistance :: Distance m (RawCoord a) -> Distance m (RawCoord (Symmetric sym a))
castDistance (Distance d) = Distance $ \(RawCoord a) -> d (RawCoord a)

{-# INLINE dist_CornerOrien_UDSlice #-}
(d_CornerOrien_UDSlice, dist_CornerOrien_UDSlice)
  = distanceWith2 "dist_CornerOrien_UDSlice" proj18CornerOrien proj18UDSlice rawCornerOrien rawUDSlice
  {-
(d_CornerOrien_LRSlice, dist_CornerOrien_LRSlice)
  = distanceWith2 "dist_CornerOrien_LRSlice" proj18CornerOrien proj18LRSlice
(d_CornerOrien_FBSlice, dist_CornerOrien_FBSlice)
  = distanceWith2 "dist_CornerOrien_FBSlice" proj18CornerOrien proj18FBSlice
-}
(d_UDSlicePermu_EdgeOrien, dist_UDSlicePermu_EdgeOrien)
  = distanceWith2 "dist_UDSlicePermu_EdgeOrien" proj18UDSlicePermu proj18EdgeOrien rawUDSlicePermu rawEdgeOrien

(d_EdgeOrien_UDSlice, dist_EdgeOrien_UDSlice)
  = distanceWith2 "dist_EdgeOrien_UDSlice" proj18EdgeOrien proj18UDSlice rawEdgeOrien rawUDSlice
  {-
(d_EdgeOrien_LRSlice, dist_EdgeOrien_LRSlice)
  = distanceWith2 "dist_EdgeOrien_LRSlice" proj18EdgeOrien proj18LRSlice
(d_EdgeOrien_FBSlice, dist_EdgeOrien_FBSlice)
  = distanceWith2 "dist_EdgeOrien_FBSlice" proj18EdgeOrien proj18FBSlice
  -}

{-# INLINE dist_UDEdgePermu2_UDSlicePermu2 #-}
(d_UDEdgePermu2_UDSlicePermu2, dist_UDEdgePermu2_UDSlicePermu2)
  = distanceWith2 "dist_EdgePermu2" proj10UDEdgePermu2 proj10UDSlicePermu2 rawUDEdgePermu2 rawUDSlicePermu2

{-# INLINE dist_CornerPermu_UDSlicePermu2 #-}
(d_CornerPermu_UDSlicePermu2, dist_CornerPermu_UDSlicePermu2)
  = distanceWith2 "dist_CornerPermu_UDSlicePermu2" proj10CornerPermu proj10UDSlicePermu2 rawCornerPermu rawUDSlicePermu2
{-
dist_CornerPermu = distanceWithCI move18CornerPermu
dist_CornerOrien = distanceWithCI move18CornerOrien
dist_EdgeOrien = distanceWithCI move18EdgeOrien

-}
