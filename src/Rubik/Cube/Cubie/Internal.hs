{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module Rubik.Cube.Cubie.Internal where

import Rubik.Cube.Facelet.Internal as F
import Rubik.Misc

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Function ( on )
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- | Cubie permutation is in replaced-by representation.
newtype CornerPermu = CornerPermu { fromCornerPermu :: Vector Int }
  deriving (Eq, Show)

newtype CornerOrien = CornerOrien { fromCornerOrien :: Vector Int }
  deriving (Eq, Show)

data Corner = Corner
  { cPermu :: CornerPermu
  , cOrien :: CornerOrien }
  deriving (Eq, Show)

-- | Check that the argument is a permutation of size 8 and wrap it.
--
-- In a 'solvable' Rubik's cube,
-- its parity must be equal to that of the associated 'EdgePermu'.
cornerPermu :: Vector Int -> Maybe CornerPermu
cornerPermu v = CornerPermu <$> mfilter check (Just v)
  where check v = U.length v == numCorners
               && isPermutationVector v

unsafeCornerPermu = CornerPermu
unsafeCornerPermu' = CornerPermu . U.fromList

-- | Check that the argument is a vector of senary (6) values of size 8 and
-- wrap it.
--
-- In a 'solvable' Rubik's cube,
-- only ternary values are possible;
-- i.e., all elements must be between 0 and 2.
-- Their sum must also be a multiple of 3.
--
-- == Orientation encoding
--
-- Corner orientations are permutations of 3 facelets.
--
-- They are mapped to integers in @[0 .. 5]@
-- such that @[0, 1, 2]@ are rotations (even permutations)
-- and @[3, 4, 5]@ are transpositions (although impossible in a Rubik's cube).
--
-- - 0. identity
-- - 1. counter-clockwise
-- - 2. clockwise
-- - 3. left facelet fixed
-- - 4. right facelet fixed
-- - 5. top (reference) facelet fixed
--
cornerOrien :: Vector Int -> Maybe CornerOrien
cornerOrien v = do
  guard $ U.length v == numCorners
       && U.all (\o -> 0 <= o && o < 6) v
  return (CornerOrien v)

unsafeCornerOrien = CornerOrien
unsafeCornerOrien' = CornerOrien . U.fromList

--

-- | Cubie permutation is in replaced-by representation.
newtype EdgePermu = EdgePermu { fromEdgePermu :: Vector Int }
  deriving (Eq, Show)

newtype EdgeOrien = EdgeOrien { fromEdgeOrien :: Vector Int }
  deriving (Eq, Show)

data Edge = Edge
  { ePermu :: EdgePermu
  , eOrien :: EdgeOrien }
  deriving (Eq, Show)

-- | Check that the argument is a permutation of size 12 and wrap it.
--
-- In a 'solvable' Rubik's cube,
-- its parity must be equal to that of the associated 'CornerPermu'.
edgePermu :: Vector Int -> Maybe EdgePermu
edgePermu v = do
  guard $ U.length v == numEdges
       && isPermutationVector v
  return (EdgePermu v)

unsafeEdgePermu = EdgePermu
unsafeEdgePermu' = EdgePermu . U.fromList

-- | Check that the argument is a vector of binary values of size 12 and wrap it.
--
-- In a 'solvable' Rubik's cube, their sum must be even.
edgeOrien :: Vector Int -> Maybe EdgeOrien
edgeOrien v = do
  guard $ U.length v == numEdges
       && U.all (`elem` [0, 1]) v
  return (EdgeOrien v)

unsafeEdgeOrien = EdgeOrien
unsafeEdgeOrien' = EdgeOrien . U.fromList

-- Complete cube

-- | A cube is given by the positions of its corners and edges.
--
-- Cubes are identified with the permutations that produce them starting
-- from the solved cube.
--
-- The cube permutation composition (@class 'Group' 'Cube'@) is defined
-- \"in left to right order\", so that the sequence of movements
-- \"@x@ then @y@ then @z@\" is represented by @x <> y <> z@.
data Cube = Cube
  { corner :: Corner
  , edge   :: Edge }
  deriving (Eq, Show)

class FromCube a where
  fromCube :: Cube -> a

instance (FromCube a, FromCube b) => FromCube (a, b) where
  fromCube c = (fromCube c, fromCube c)

-- | Group action of 'Cube' on type @a@
--
-- >  x `cubeAction` iden == x
-- > (x `cubeAction` a) `cubeAction` b == x `cubeAction (a <> b)
--
-- It seems that with proper additional laws
-- between 'FromCube' and 'Group' instances,
-- it may be possible to automatically deduce a default 'CubeAction' instance.
--
-- > cubeAction a = (a <>) . fromCube
--
-- This module defines representations of right cosets (@Hg where g :: Cube@)
-- of certain subgroups H of the Rubik group @Cube@, which acts on the right of
-- the set of cosets.
class CubeAction a where
  cubeAction :: a -> Cube -> a

instance (CubeAction a, CubeAction b) => CubeAction (a, b) where
  cubeAction (a, b) c = (cubeAction a c, cubeAction b c)

cube :: Vector Int -> Vector Int -> Vector Int -> Vector Int -> Maybe Cube
cube cp co ep eo = Cube <$> c <*> e
  where c = Corner <$> cornerPermu cp <*> cornerOrien co
        e = Edge <$> edgePermu ep <*> edgeOrien eo

cube' :: [Int] -> [Int] -> [Int] -> [Int] -> Maybe Cube
cube' cp co ep eo = cube (f cp) (f co) (f ep) (f eo)
  where f = U.fromList

unsafeCube :: Vector Int -> Vector Int -> Vector Int -> Vector Int -> Cube
unsafeCube cp co ep eo = Cube c e
  where c = Corner (CornerPermu cp) (CornerOrien co) -- Unsafe raw constructors
        e = Edge (EdgePermu ep) (EdgeOrien eo)

unsafeCube' :: [Int] -> [Int] -> [Int] -> [Int] -> Cube
unsafeCube' cp co ep eo = unsafeCube (f cp) (f co) (f ep) (f eo)
  where f = U.fromList

--

instance FromCube Corner where
  fromCube = corner

instance FromCube CornerPermu where
  fromCube = cPermu . corner

instance FromCube CornerOrien where
  fromCube = cOrien . corner

instance FromCube Edge where
  fromCube = edge

instance FromCube EdgePermu where
  fromCube = ePermu . edge

instance FromCube EdgeOrien where
  fromCube = eOrien . edge

--

-- | > numCorners = 8
numCorners = 8 :: Int

-- | > numEdges = 12
numEdges = 12 :: Int

-- Apply @o@ then @o'@ (as permutation of facelets, from the reference position)
o `oPlus` o' | o < 3 && o' < 3 =      (o + o') `mod` 3
             | o < 3           = 3 + ((o'+ o)  `mod` 3)
             |          o' < 3 = 3 + ((o - o')  `mod` 3)
             | otherwise       =      (o - o') `mod` 3

oInv o | o == 0    = 0
       | o < 3     = 3 - o
       | otherwise = o

--

instance Semigroup CornerPermu where
  CornerPermu b <> CornerPermu c = CornerPermu $ composeVector b c

instance Monoid CornerPermu where
  mempty = CornerPermu $ idVector numCorners
  mappend = (<>)

instance Group CornerPermu where
  inverse (CornerPermu a) = CornerPermu $ inverseVector a


instance Semigroup EdgePermu where
  EdgePermu b <> EdgePermu c = EdgePermu $ composeVector b c

instance Monoid EdgePermu where
  mempty = EdgePermu $ idVector numEdges
  mappend = (<>)

instance Group EdgePermu where
  inverse (EdgePermu a) = EdgePermu $ inverseVector a


instance CubeAction CornerPermu where
  cubeAction cp_ = (cp_ <>) . fromCube

instance CubeAction EdgePermu where
  cubeAction ep_ = (ep_ <>) . fromCube

-- Helper function to define the action of 'Cube' on 'CornerOrien'
actionCorner :: CornerOrien -> Corner -> CornerOrien
actionCorner (CornerOrien o) (Corner (CornerPermu gp) (CornerOrien go))
  = CornerOrien $ U.zipWith oPlus (U.backpermute o gp) go

-- Helper function to define the action of 'Cube' on 'EdgeOrien'
actionEdge :: EdgeOrien -> Edge -> EdgeOrien
actionEdge (EdgeOrien o) (Edge (EdgePermu gp) (EdgeOrien go))
  = EdgeOrien $ U.zipWith (((`mod` 2) .) . (+)) (U.backpermute o gp) go

instance CubeAction CornerOrien where
  cubeAction co_ = actionCorner co_ . corner

instance CubeAction EdgeOrien where
  cubeAction eo_ = actionEdge eo_ . edge

--

instance CubeAction Corner where
  cubeAction (Corner cp co) c =
    Corner (cp `cubeAction` c) (co `cubeAction` c)

instance CubeAction Edge where
  cubeAction (Edge ep eo) c =
    Edge (ep `cubeAction` c) (eo `cubeAction` c)

--

instance Semigroup Corner where
  (<>)    (Corner bp_ bo_)
        c@(Corner cp_ co_)
    =      Corner dp_ do_
    where dp_ = bp_ <>             cp_
          do_ = bo_ `actionCorner` c

instance Monoid Corner where
  mempty = Corner iden idCornerO
    where idCornerO = CornerOrien $ U.replicate numCorners 0
  mappend = (<>)

instance Group Corner where
  inverse (Corner ap_  (CornerOrien ao))
    =      Corner ap_' (CornerOrien ao')
    where ap_'@(CornerPermu ap') = inverse ap_
          ao'                    = U.map oInv . U.backpermute ao $ ap'

instance Semigroup Edge where
  (<>)    (Edge bp_ bo_)
        c@(Edge cp_ co_)
    =      Edge dp_ do_
    where dp_ = bp_ <>           cp_
          do_ = bo_ `actionEdge` c

instance Monoid Edge where
  mempty = Edge iden idEdgeO
    where idEdgeO = EdgeOrien $ U.replicate numEdges 0
  mappend = (<>)

instance Group Edge where
  inverse (Edge ap_  (EdgeOrien ao))
    =      Edge ap_' (EdgeOrien ao')
    where ap_'@(EdgePermu ap') = inverse ap_
          ao'                  = U.backpermute ao ap'

--

instance Semigroup Cube where
  Cube cA eA <> Cube cB eB = Cube (cA <> cB) (eA <> eB)

instance Monoid Cube where
  mempty = Cube iden iden
  mappend = (<>)

instance Group Cube where
  inverse (Cube c e) = Cube (inverse c) (inverse e)

--

-- | Tests whether a cube can be solved with the standard set of moves.
solvable :: Cube -> Bool
solvable (Cube (Corner (CornerPermu cp) (CornerOrien co))
               (Edge   (EdgePermu   ep) (EdgeOrien   eo))) =
  signPermutationVector cp == signPermutationVector ep
  && U.sum co `mod` 3 == 0
  && U.all (< 3) co
  -- Above: the data structure allows to encode all 6 permutations of the 3 facelets
  -- so we need to exclude the 3 transpositions, which are represented by 3, 4, 5.
  && U.sum eo `mod` 2 == 0

-- Conversions

-- Facelet conversion

-- | 0 <= o < 6
symRotate :: Int -> [Int] -> [Int]
symRotate o
  | o < 3     = rotate o             -- Even permutation
  | otherwise = rotate (5 - o) . sym -- Odd permutation
  where sym [a,b,c] = [a,c,b]

toFacelet :: Cube -> Facelets
toFacelet
  (Cube
    { corner = Corner (CornerPermu cp) (CornerOrien co)
    , edge   = Edge (EdgePermu ep) (EdgeOrien eo) })
  = unsafeFacelets $ U.create (do
      v <- MU.new F.numFacelets
      setFacelets v cp co cornerFacelets         -- Corners
      setFacelets v ep eo edgeFacelets           -- Edges
      forM_ [4, 13 .. 49] (\x -> MU.write v x x) -- Centers
      return v)
  where
    -- Return an association list
    -- (i, j) <- assoc
    -- such that in the cube facelet i is replaced by facelet j
    -- p: Cubie permutations
    -- o: Cubie orientations
    -- f: Cubie facelets
    -- Parameterized over a choice of cubie family (edges/corners)
    setFacelets v p o f
      = forM_
          ((zip `on` concat) f orientedFaces)
        . uncurry $ MU.write v
      where
        orientedFaces = zipWith symRotate (U.toList o) cubieFacelets
        cubieFacelets = map (f !!) (U.toList p)

-- | Convert from facelet to cubie permutation.
--
-- Evaluates to a 'Left' error if a combination of colors does not correspond to
-- a regular cubie from the solved cube: the colors of the facelets on one
-- cubie must be unique, and must not contain facelets of opposite faces.
-- The error is the list of indices of facelets of such an invalid cubie.
--
-- Another possible error is that the resulting configuration is not a
-- permutation of cubies (at least one cubie is absent, and one is duplicated).
-- In that case, the result is 'Right' 'Nothing'.
colorFaceletsToCube :: ColorFacelets -> Either [Int] (Maybe Cube)
colorFaceletsToCube (fromColorFacelets -> c) = do
  (co, cp) <- pack <$> zipWithM findCorner (colorsOfC cornerFacelets) cornerFacelets
  (eo, ep) <- pack <$> zipWithM findEdge (colorsOfC edgeFacelets) edgeFacelets
  Right $ cube cp co ep eo
  where
    pack = U.unzip . U.fromList
    colorsOfC = (((c U.!) <$>) <$>)
    findCorner = findPos cornerColors [0 .. 5]
    findEdge   = findPos edgeColors [0, 1]
    cornerColors = (colorOf <$>) <$> cornerFacelets
    edgeColors = (colorOf <$>) <$> edgeFacelets
    -- @xs@ is a list of color patterns, @x@ is one pattern,
    -- @os@ is a list of permutation indices (orientations).
    -- (identity + symmetry for edges,
    -- identity + 2 rotations + 3 symmetries for corners)
    -- The result @(o, i)@ is the pair of indices of the corresponding
    -- orientation and pattern in @os@ and @xs@, such that
    -- > symRotate o (xs !! i) = x
    -- An error is returned otherwise
    findPos :: [[Int]] -> [Int] -> [Int] -> e -> Either e (Int, Int)
    findPos xs os x e
      = case join . find isJust $
          map
            (\o -> (,) o <$> elemIndex x (map (symRotate o) xs))
            os
        of
          Nothing -> Left e
          Just x -> Right x

stringOfCubeColors :: Cube -> String
stringOfCubeColors =  stringOfColorFacelets' . toFacelet

--

-- ** UDSlice

-- | Position of the 4 UDSlice edges (carried-to)
newtype UDSlicePermu = UDSlicePermu { fromUDSlicePermu :: Vector Int }
  deriving (Eq, Show)

-- | Position of the 4 UDSlice edges up to permutation (carried-to).
-- The vector is always sorted.
newtype UDSlice = UDSlice { fromUDSlice :: Vector Int }
  deriving (Eq, Show)

-- | Position of the 4 UDSlice edges (replaced-by),
-- assuming they are all in that slice already.
newtype UDSlicePermu2 = UDSlicePermu2 { fromUDSlicePermu2 :: Vector Int }
  deriving (Eq, Show)

-- | Position of the 8 other edges (replaced-by),
-- assuming UDSlice edges are in that slice already.
newtype UDEdgePermu2 = UDEdgePermu2 { fromUDEdgePermu2 :: Vector Int }
  deriving (Eq, Show)

type FlipUDSlice = (UDSlice, EdgeOrien)
type FlipUDSlicePermu = (UDSlicePermu, EdgeOrien)

-- | > numUDSliceEdges = 4
numUDSliceEdges = 4 :: Int

unsafeUDSlicePermu = UDSlicePermu
unsafeUDSlicePermu' = UDSlicePermu . U.fromList

uDSlicePermu :: Vector Int -> Maybe UDSlicePermu
uDSlicePermu v = do
  guard $ U.length v == numUDSliceEdges
       && U.all (liftA2 (&&) (0 <=) (< numEdges)) v
       && (length . nub . U.toList) v == numUDSliceEdges
  return (UDSlicePermu v)

-- | Wrap an increasing list of 4 elements in @[0 .. 11]@.
uDSlice :: Vector Int -> Maybe UDSlice
uDSlice v = do
  guard $ U.length v == numUDSliceEdges
       && U.and (U.zipWith (<) ((-1) `U.cons` v) (v `U.snoc` 12))
  return (UDSlice v)

unsafeUDSlice = UDSlice
unsafeUDSlice' = UDSlice . U.fromList

-- | Wrap a permutation of size 4.
uDSlicePermu2 :: Vector Int -> Maybe UDSlicePermu2
uDSlicePermu2 v = do
  guard $ U.length v == numUDSliceEdges
       && isPermutationVector v
  return (UDSlicePermu2 v)

unsafeUDSlicePermu2 = UDSlicePermu2
unsafeUDSlicePermu2' = UDSlicePermu2 . U.fromList

-- | Wrap a permutation of size 8.
uDEdgePermu2 :: Vector Int -> Maybe UDEdgePermu2
uDEdgePermu2 v = do
  guard $ U.length v == numEdges - numUDSliceEdges
       && isPermutationVector v
  return (UDEdgePermu2 v)

unsafeUDEdgePermu2 = UDEdgePermu2
unsafeUDEdgePermu2' = UDEdgePermu2 . U.fromList

vSort = U.fromList . sort . U.toList

unpermuUDSlice :: UDSlicePermu -> UDSlice
unpermuUDSlice = UDSlice . vSort . fromUDSlicePermu

edgePermu2 :: UDSlicePermu2 -> UDEdgePermu2 -> EdgePermu
edgePermu2 (UDSlicePermu2 sp) (UDEdgePermu2 ep)
  = EdgePermu (ep U.++ U.map (+8) sp)

-- Projections of the identity cube
neutralUDSlicePermu = UDSlicePermu $ U.enumFromN 8 numUDSliceEdges -- 4
neutralUDSlice = UDSlice $ U.enumFromN 8 numUDSliceEdges -- 4
neutralUDSlicePermu2 = UDSlicePermu2 $ U.enumFromN 0 numUDSliceEdges -- 4
neutralUDEdgePermu2 = UDEdgePermu2 $ U.enumFromN 0 (numEdges - numUDSliceEdges) -- 8

actionUDSlicePermu' :: EdgePermu -> Vector Int -> Vector Int
actionUDSlicePermu' (EdgePermu ep) = U.map (fromJust . flip U.elemIndex ep)

actionUDSlicePermu :: UDSlicePermu -> EdgePermu -> UDSlicePermu
actionUDSlicePermu (UDSlicePermu p) ep
  = UDSlicePermu (actionUDSlicePermu' ep p)

actionUDSlice :: UDSlice -> EdgePermu -> UDSlice
actionUDSlice (UDSlice s) ep = UDSlice (act s)
  where
    act = vSort . actionUDSlicePermu' ep

-- 'EdgePermu' should leave UDSlice stable.
actionUDSlicePermu2 :: UDSlicePermu2 -> EdgePermu -> UDSlicePermu2
actionUDSlicePermu2 (UDSlicePermu2 sp) (EdgePermu ep) =
  UDSlicePermu2 $ sp `composeVector` U.map (subtract 8) (U.drop 8 ep)

-- 'EdgePermu' should leave UDSlice stable.
actionUDEdgePermu2 :: UDEdgePermu2 -> EdgePermu -> UDEdgePermu2
actionUDEdgePermu2 (UDEdgePermu2 ep') (EdgePermu ep) =
  UDEdgePermu2 $ ep' `composeVector` U.take 8 ep

instance CubeAction UDSlicePermu where
  cubeAction p = actionUDSlicePermu p . fromCube

instance CubeAction UDSlice where
  cubeAction s = actionUDSlice s . fromCube

instance CubeAction UDSlicePermu2 where
  cubeAction sp = actionUDSlicePermu2 sp . fromCube

instance CubeAction UDEdgePermu2 where
  cubeAction e = actionUDEdgePermu2 e . fromCube

instance FromCube UDSlicePermu where
  fromCube = cubeAction neutralUDSlicePermu

instance FromCube UDSlice where
  fromCube = cubeAction neutralUDSlice

instance FromCube UDSlicePermu2 where
  fromCube = cubeAction neutralUDSlicePermu2

instance FromCube UDEdgePermu2 where
  fromCube = cubeAction neutralUDEdgePermu2

-- TODO: Make a type class of this (?)
-- | The conjugation is only compatible when the 'Cube' symmetry
-- leaves UDSlice edges stable, and either flips them all or none of them,
-- and either flips all 8 non-UDSlice edges or none of them.
conjugateFlipUDSlice :: Cube -> FlipUDSlice -> FlipUDSlice
conjugateFlipUDSlice c = assert conjugable conjugate
  where
    (EdgeOrien eo_c, EdgePermu ep_c) = fromCube c
    conjugable
      = let fromCube_c = UDSlice . vSort . U.drop 8 $ ep_c
        in fromCube_c == neutralUDSlice
           && isConstant (U.take 8 eo_c)
           && isConstant (U.drop 8 eo_c)
    isConstant v = U.init v == U.tail v
    udsO = eo_c U.! 8
    altO = eo_c U.! 0
    conjugate (uds_@(UDSlice uds), EdgeOrien eo) = (uds_', EdgeOrien eo')
      where
        eo' = U.zipWith
                (\o p -> (o + eo U.! p + bool altO udsO (p `U.elem` uds)) `mod` 2)
                eo_c
                ep_c
        uds_' = cubeAction uds_ c

-- | Expects UDSlice-stable symmetry
conjugateFlipUDSlicePermu :: Cube -> FlipUDSlicePermu -> FlipUDSlicePermu
conjugateFlipUDSlicePermu c = assert conjugable conjugate
  where
    (EdgeOrien eo_c, EdgePermu ep_c) = fromCube c
    udsp_c = U.drop 8 ep_c
    conjugable
      = UDSlicePermu (vSort udsp_c) == neutralUDSlicePermu
      && isConstant (U.take 8 eo_c)
      && isConstant (U.drop 8 eo_c)
    isConstant v = U.init v == U.tail v
    conjugate fuds@(udsp, _)
      = (conjugateUDSlicePermu c udsp, conjugateEdgeOrien' c fuds)

conjugateEdgeOrien' :: Cube -> FlipUDSlicePermu -> EdgeOrien
conjugateEdgeOrien' c (UDSlicePermu udsp, EdgeOrien eo)
  = EdgeOrien $ U.zipWith
      (\o p -> (o + eo U.! p + bool altO udsO (p `U.elem` udsp)) `mod` 2)
      eo_c
      ep_c
  where
    (EdgeOrien eo_c, EdgePermu ep_c) = fromCube c
    udsO = eo_c U.! 8
    altO = eo_c U.! 0

conjugateUDSlicePermu :: Cube -> UDSlicePermu -> UDSlicePermu
conjugateUDSlicePermu c (UDSlicePermu udsp)
  = cubeAction (UDSlicePermu $ U.map (\i -> udsp U.! (i - 8)) udsp_c) c
  where
    EdgePermu ep_c = fromCube c
    udsp_c = U.drop 8 . fromEdgePermu $ fromCube c

-- | Expects UDSlice-stable symmetry.
conjugateCornerOrien :: Cube -> CornerOrien -> CornerOrien
conjugateCornerOrien c (CornerOrien co) = cubeAction (CornerOrien (U.map (oPlus (oInv o)) co)) c
  where
    CornerOrien co_c = fromCube c
    o = U.head co_c

