{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators, ViewPatterns #-}
module Rubik.Solver where

import Rubik.Cube
import Rubik.Distances
import Rubik.IDA
import Rubik.Misc
import Rubik.Symmetry

import Control.Applicative

import Data.Coerce
import Data.Foldable
import Data.Tuple.Extra
import qualified Data.Vector as V
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

infixr 4 |*|, |.|

{-# INLINE (|*|) #-}
(|*|) :: (TupleCons b0, TupleCons bs, TupleCons b)
  => Projection x a0 as a
  -> Projection x b0 bs b
  -> Projection x (a0 :| b0) (as :| bs) (a :| b)
a |*| b = Projection
  { convertP = liftA2 (|:|) (convertP a) (convertP b)
  , isIdenP = \(split -> (a_, b_)) -> isIdenP a a_ && isIdenP b b_
  , indexP = \(split -> (as_, bs_)) (split -> (a_, b_)) -> indexP a as_ a_ |:| indexP b bs_ b_
  , subIndexSize = subIndexSize a * subIndexSize b
  , unfoldP = \(split -> (a0_, b0_)) ci ->
      let (ai, bi) = ci `divMod` subIndexSize b
      in zipWith (|:|) (unfoldP a a0_ ai) (unfoldP b b0_ bi)
  , subIndexP = \(split -> (a_, b_)) -> flatIndex (subIndexSize b) (subIndexP a a_) (subIndexP b b_) }

{-# INLINE (|.|) #-}
(|.|) :: forall x a0 as a b0 bs b
  . Projection x a0 as a
  -> Projection x b0 bs b
  -> Projection x (a0, b0) (as, bs) (a, b)
a |.| b = a |*| (coerce b :: Projection x (Tuple1 b0) (Tuple1 bs) (Tuple1 b))

{-# INLINE (>$<) #-}
(>$<) :: forall m a b. (b -> a) -> Distance m a -> Distance m b
(>$<) = coerce (flip (.) :: (b -> a) -> (a -> DInt) -> (b -> DInt))

{-# INLINE maxDistance #-}
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

{-# INLINE mkSearch #-}
mkSearch
  :: Eq a
  => MoveTag m [ElemMove] -> a0
  -> Projection Cube a0 as a
  -> Distance m a
  -> Search [] DInt ElemMove (Tag a)
mkSearch (MoveTag moveNames) ms ps pd = Search
  { goal = isIdenP ps . snd
  , estm = distanceP pd . snd
  , edges = \(i, t) -> fmap
              (\(l, succs, j') ->
                let x = indexP ps succs t in Succ l 1 (j', x))
              (succVector V.! (subIndexP ps t * 7 + i))
  }
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

{-# INLINE rawProjection #-}
rawProjection :: (FromCube a, RawEncodable a) => Projection' m a
rawProjection = Projection
  { convertP = convert
  , isIdenP = (== convert iden)
  , indexP = (!$)
  , subIndexSize = 1
  , unfoldP = \(MoveTag as) _ -> as
  , subIndexP = \_ -> 0
  }
  where
    convert = encode . fromCube

{-# INLINE symProjection #-}
symProjection :: (FromCube a, RawEncodable a)
  => (a -> SymCoord sym a) -> SymProjection m sym a
symProjection convert = Projection
  { convertP = convert'
  , isIdenP = let (x0, _) = convert' iden in \(x, _) -> x == x0
  , indexP = symMove' 16
  , subIndexSize = 16
  , unfoldP = \(MoveTag as) i -> [ as !! j | j <- symAsMovePerm (sym16 !! i) ]
  , subIndexP = \(_, SymCode i) -> i
  }
  where
    convert' = convert . fromCube

-- TODO newtype this
{-# INLINE symmetricProj #-}
symmetricProj :: Eq c => Symmetry sym
  -> Projection Cube (MoveTag m [b]) as c
  -> Projection Cube (MoveTag m [b]) as c
symmetricProj sym proj = proj
  { convertP = convert
  , unfoldP = \as i -> rawMoveSym sym (unfoldP proj as i)
  }
  where
    convert = convertP proj . conjugate (inverse (symAsCube sym))

{-# INLINE distanceWith2 #-}
distanceWith2
  :: (RawEncodable a, RawEncodable b)
  =>  P.Vector DInt -> Distance m (RawCoord a, RawCoord b)
distanceWith2 v = Distance $ \(RawCoord a_, b@(RawCoord b_)) ->
  v P.! flatIndex (range b) a_ b_
