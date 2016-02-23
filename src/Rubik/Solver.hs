{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, ViewPatterns #-}
module Rubik.Solver where

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
import qualified Data.Vector.Primitive as P

type MaybeFace = Int
type SubIndex = Int

data Projection x a0 as a = Projection
  { convertP :: x -> a
  , isIdenP :: a -> Bool
  , indexP :: as -> a -> a
  , subIndexSize :: Int
  , unfoldP :: a0 -> SubIndex -> [as]
  , subIndexP :: a -> SubIndex
  }

type Projection' m a = Projection Cube (MoveTag m [RawMove a]) (RawMove a) (RawCoord a)
type SymProjection m sym a = Projection Cube (MoveTag m [SymMove sym a]) (SymMove sym a) (SymCoord sym a)

newtype Distance m a = Distance { distanceP :: a -> DInt }

infixr 4 |:|, |.|

(|:|) :: (TupleCons a0 b0 c0, TupleCons as bs cs, TupleCons a b c)
  => Projection x a0 as a -> Projection x b0 bs b -> Projection x c0 cs c
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

(|.|) :: Projection x a0 as a -> Projection x b0 bs b
  -> Projection x (Tuple2 a0 b0) (Tuple2 as bs) (Tuple2 a b)
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
  -> Projection Cube a0 as a
  -> Distance m a
  -> Search [] DInt ElemMove (Tag a)
mkSearch (MoveTag moveNames) ms ps pd = Search
  { goal = isIdenP ps . snd
  , estm = distanceP pd . snd
  , edges = \(i, t) -> fmap
              (\(l, succs, j') ->
                let x = indexP ps succs t in x `seq` Succ l 1 (j', x))
              (succVector V.! (subIndexP ps t * 7 + i)) }
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

symProjection :: FromCube a => RawEncoding a -> (a -> SymCoord sym a) -> SymProjection m sym a
symProjection enc convert = Projection
  { convertP = convert'
  , isIdenP = let (x0, _) = convert' iden in \(x, _) -> x == x0
  , indexP = symMove' 16
  , subIndexSize = 16
  , unfoldP = \(MoveTag as) i -> [ as !! j | j <- symAsMovePerm (sym16 !! i) ]
  , subIndexP = \(_, SymCode i) -> i
  } where convert' = convert . fromCube

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

projCornerOrien = rawProjection rawCornerOrien

projCornerPermu = rawProjection rawCornerPermu

projEdgeOrien = rawProjection rawEdgeOrien

projUDSlicePermu = rawProjection rawUDSlicePermu

{-
projLRSlicePermu = symmetricProj move18UDSlicePermu rawUDSlicePermu symmetry_urf3
projFBSlicePermu = symmetricProj move18UDSlicePermu rawUDSlicePermu symmetry_urf3'
-}

projUDSlice = rawProjection rawUDSlice

{-
projLRSlice = symmetricProj move18UDSlice rawUDSlice symmetry_urf3
projFBSlice = symmetricProj move18UDSlice rawUDSlice symmetry_urf3'
-}

-- ** 10 moves (G1 group)

projUDSlicePermu2 = rawProjection rawUDSlicePermu2

projUDEdgePermu2 = rawProjection rawUDEdgePermu2

distanceWith2 :: P.Vector DInt -> RawEncoding b -> Distance m (RawCoord a, RawCoord b)
distanceWith2 v (range -> n) = Distance $ \(RawCoord a, RawCoord b) -> v P.! flatIndex n a b

