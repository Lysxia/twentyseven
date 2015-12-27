{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, ViewPatterns #-}
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
import Data.Coerce
import Data.Foldable
import Data.Int ( Int8 )
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.StrictTuple
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

type MaybeFace = Int
type SubIndex = Int

data Projection a0 as a = Projection
  { convertP :: Cube -> a
  , isIdenP :: a -> Bool
  , indexP :: as -> a -> a
  , subIndexSize :: Int
  , unfoldP :: a0 -> SubIndex -> [as]
  , subIndexP :: a -> SubIndex
  }

type Projection' m a = Projection (MoveTag m [RawMove a]) (RawMove a) (RawCoord a)

newtype Distance m a = Distance { distanceP :: a -> DInt }

infixr 4 |:|, |.|

(|:|) :: (TupleCons a0 b0 c0, TupleCons as bs cs, TupleCons a b c)
  => Projection a0 as a -> Projection b0 bs b -> Projection c0 cs c
{-# INLINE (|:|) #-}
a |:| b = Projection
  { convertP = liftA2 (|*|) (convertP a) (convertP b)
  , isIdenP = \(split -> (a_, b_)) -> isIdenP a a_ && isIdenP b b_
  , indexP = \(split -> (as_, bs_)) (split -> (a_, b_)) -> indexP a as_ a_ |*| indexP b bs_ b_
  , subIndexSize = subIndexSize a * subIndexSize b
  , unfoldP = \(split -> (a0_, b0_)) ci ->
      let (ai, bi) = ci `divMod` subIndexSize b
      in zipWith (|*|) (unfoldP a a0_ ai) (unfoldP b b0_ bi)
  , subIndexP = \(split -> (a_, b_)) -> flatIndex (subIndexSize b) (subIndexP a a_) (subIndexP b b_) }

(|.|) :: Projection a0 as a -> Projection b0 bs b
  -> Projection (Tuple2 a0 b0) (Tuple2 as bs) (Tuple2 a b)
a |.| b = a |:| coerce b

(>$<) :: forall m a b. (b -> a) -> Distance m a -> Distance m b
f >$< Distance g = Distance (g . f)
--(>$<) = coerce (flip (.) :: (b -> a) -> (a -> DInt) -> (b -> DInt))

maxDistance :: forall f m a. Foldable f => f (Distance m a) -> Distance m a
maxDistance = foldl' (\(Distance f) (Distance g) -> Distance $ \x -> max (f x) (g x)) (Distance $ const 0)

-- | ==Branching reduction
--
-- The @Int@ projection keeps track of the latest move (@== 6@
-- for the starting point).
--
-- 18 moves
--
-- We can indeed reduce the branching factor from 18 to 15
-- by considering that successive moves on the same face
-- can and will be shortened as a single move.
--
-- Furthermore, since moves on opposite faces commute, we may force
-- them to be in an arbitrary order, reducing the branching factor
-- to 12 after half of the moves (U, L, F).
--
-- 10 moves
--
-- Instead of a factor 10, we have factors
--
-- - 9 after R, B;
-- - 8 after L, F;
-- - 7 after D;
-- - 4 after U.

mkSearch
  :: (Eq a)
  => MoveTag m [ElemMove] -> a0
  -> Projection a0 as a
  -> Distance m a
  -> Search [] DInt ElemMove (Tag a)
mkSearch (MoveTag moveNames) ms ps pd = Search
  { goal = isIdenP ps . snd
  , estm = distanceP pd . snd
  , edges = \(i, t) -> fmap
              (\(l, succs, j') ->
                let x = indexP ps succs t in x `seq` Succ l 1 (j', x))
              (succVector V.! ({-subIndexP ps t * 7 + -} i)) }
  where
    -- For every move, filter out "larger" moves for an arbitrary total order of faces
    succVector = V.fromList $ do
      subi <- [0 .. subIndexSize ps - 1]
      let as = unfoldP ps ms subi
      i' <- [0 .. 6]
      return
        [ (l, m, fromEnum j)
        | (l@(_, j), m) <- zip moveNames as
        , i' == 6 || (let i = toEnum i' in not (i == j || oppositeAndGT j i)) ]

-- | Distances only go up to 20 for 3x3 Rubik's cubes.
type DInt = Int8

type Tag a = (Int, a)

tag :: a -> Tag a
tag = (,) 6

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

rawMoveTables :: CubeAction a => MoveTag m [Cube] -> RawEncoding a -> MoveTag m [RawMove a]
rawMoveTables (MoveTag moves) enc = MoveTag $ moveTable enc <$> moves

storedRawMoveTables :: CubeAction a => String -> MoveTag m [Cube] -> RawEncoding a
  -> Store (MoveTag m [RawMove a])
storedRawMoveTables name moves enc = store name (rawMoveTables moves enc)

rawProjection :: FromCube a => RawEncoding a -> Projection' m a
{-# INLINE rawProjection #-}
rawProjection enc = Projection
  { convertP = convert
  , isIdenP = (== convert iden)
  , indexP = (!$)
  , subIndexSize = 1
  , unfoldP = \(MoveTag as) _ -> as
  , subIndexP = \_ -> 0 }
  where
    convert = encode enc . fromCube

type SymProjection sym a
  = Projection [SymMove sym a] (SymMove sym a) (SymClass sym a, SymCode sym)

{-
symProjection :: FromCube a => RawEncoding a -> SymProjection sym a
symProjection enc = Projection
  { convertP = convert
  , isIdenP = let (
-}
{-
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

move18CornerOrien
  = storedRawMoveTables "move18CornerOrien" move18 rawCornerOrien
projCornerOrien
  = rawProjection rawCornerOrien

move18CornerPermu
  = storedRawMoveTables "move18CornerPermu" move18 rawCornerPermu
projCornerPermu
  = rawProjection rawCornerPermu

move18EdgeOrien
  = storedRawMoveTables "move18EdgeOrien" move18 rawEdgeOrien
projEdgeOrien = rawProjection rawEdgeOrien

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

actionFlipUDSlicePermu = Action (conjugateFlipUDSlicePermu <$> sym16')
symReprFlipUDSlicePermu = symClasses rawFlipUDSlicePermu actionFlipUDSlicePermu

move18SymFlipUDSlicePermu = storedSymMoveTables "move10symFlipUDSlicePermu" move18
  rawFlipUDSlicePermu actionFlipUDSlicePermu symReprFlipUDSlicePermu

move18to10 :: MoveTag Move18 [as] -> MoveTag Move10 [as]
move18to10 (MoveTag as) = MoveTag
  (composeList as [ n - 1 + 3 * fromEnum m | (n, m) <- unMoveTag move10Names ])

-- ** 10 moves (G1 group)

move10CornerPermu = store "move10CornerPermu" $ move18to10 (value move18CornerPermu)
move10UDSlicePermu2
  = storedRawMoveTables "move10UDSlicePermu2" move10 rawUDSlicePermu2
projUDSlicePermu2 = rawProjection rawUDSlicePermu2

move10UDEdgePermu2
  = storedRawMoveTables "move10UDEdgePermu2" move10 rawUDEdgePermu2
projUDEdgePermu2 = rawProjection rawUDEdgePermu2

storedSymMoveTables :: String -> MoveTag m [Cube] -> RawEncoding a
  -> Action sym a -> SymReprTable sym a -> (Cube -> a -> a)
  -> Store (MoveTag m [SymMove sym a])
storedSymMoveTables name (MoveTag moves) enc action reps conj
  = store name (MoveTag [ symMoveTable enc action reps (conj c) | c <- moves ])

-- * Distance tables

distanceTable2
  :: String -> Store (MoveTag m [RawMove a]) -> Store (MoveTag m [RawMove b])
  -> Projection' m a -> Projection' m b -> RawEncoding a -> RawEncoding b
  -> Store (Vector DInt)
distanceTable2 name (value -> m1) (value -> m2) proj1 proj2 (range -> n1) (range -> n2)
  = store name (distanceWith2' m1 m2 proj1 proj2 n1 n2)

distanceWith2 :: Vector DInt -> RawEncoding b -> Distance m (RawCoord a, RawCoord b)
distanceWith2 v (range -> n) = Distance $ \(RawCoord a, RawCoord b) -> v U.! flatIndex n a b

distanceWith2'
  :: MoveTag m [RawMove a] -> MoveTag m [RawMove b]
  -> Projection' m a -> Projection' m b -> Int -> Int -> Vector DInt
distanceWith2' (MoveTag m1) (MoveTag m2) proj1 proj2 n1 n2 = distances n root neighbors
  where
    n = n1 * n2
    root = flatIndex n2 (unRawCoord (convertP proj1 iden)) (unRawCoord (convertP proj2 iden))
    neighbors ((`divMod` n2) -> (x1, x2))
      = zipWith (\v1 v2 -> flatIndex n2
          (unRawCoord . indexP proj1 v1 $ RawCoord x1)
          (unRawCoord . indexP proj2 v2 $ RawCoord x2)) m1 m2

castDistance :: Distance m (RawCoord a) -> Distance m (RawCoord (Symmetric sym a))
castDistance = coerce

d_CornerOrien_UDSlice
  = distanceTable2 "dist_CornerOrien_UDSlice" move18CornerOrien move18UDSlice projCornerOrien projUDSlice rawCornerOrien rawUDSlice

{-
d_CornerOrien_LRSlice
  = distanceTable2 "dist_CornerOrien_LRSlice" projCornerOrien projLRSlice
d_CornerOrien_FBSlice
  = distanceTable2 "dist_CornerOrien_FBSlice" projCornerOrien projFBSlice

d_UDSlicePermu_EdgeOrien
  = distanceTable2 "dist_UDSlicePermu_EdgeOrien" move18UDSlicePermu move18EdgeOrien projUDSlicePermu projEdgeOrien rawUDSlicePermu rawEdgeOrien
-}

d_EdgeOrien_UDSlice
  = distanceTable2 "dist_EdgeOrien_UDSlice" move18EdgeOrien move18UDSlice projEdgeOrien projUDSlice rawEdgeOrien rawUDSlice

{-
d_EdgeOrien_LRSlice
  = distanceTable2 "dist_EdgeOrien_LRSlice" projEdgeOrien projLRSlice
d_EdgeOrien_FBSlice
  = distanceTable2 "dist_EdgeOrien_FBSlice" projEdgeOrien projFBSlice
-}

d_UDEdgePermu2_UDSlicePermu2
  = distanceTable2 "dist_EdgePermu2" move10UDEdgePermu2 move10UDSlicePermu2 projUDEdgePermu2 projUDSlicePermu2 rawUDEdgePermu2 rawUDSlicePermu2

d_CornerPermu_UDSlicePermu2
  = distanceTable2 "dist_CornerPermu_UDSlicePermu2" move10CornerPermu move10UDSlicePermu2 projCornerPermu projUDSlicePermu2 rawCornerPermu rawUDSlicePermu2

{-
dist_CornerPermu = distanceWithCI move18CornerPermu
dist_CornerOrien = distanceWithCI move18CornerOrien
dist_EdgeOrien = distanceWithCI move18EdgeOrien
-}
