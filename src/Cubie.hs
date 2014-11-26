{- |
   Cubie representation.

   A Rubik's cube is the cartesian product of a permutation of cubies
   and an action on their orientations.
-}

{-# LANGUAGE ViewPatterns #-}
module Cubie (
  -- * Complete cube
  CubeAction (..),
  FromCube (..),
  Cube (..),
  unsafeCube,
  unsafeCube',

  -- ** Solvability test
  solvable,

  -- * Corners
  numCorners,
  CornerPermu,
  CornerOrien,
  Corner (..),

  -- ** (De)construction
  cornerPermu,
  cornerOrien,
  unsafeCornerPermu,
  unsafeCornerOrien,
  fromCornerPermu,
  fromCornerOrien,

  -- * Edges
  numEdges,
  EdgePermu,
  EdgeOrien,
  Edge (..),

  -- ** (De)construction
  edgePermu,
  edgeOrien,
  unsafeEdgePermu,
  unsafeEdgeOrien,
  fromEdgePermu,
  fromEdgeOrien,

  -- * Conversions
  stringOfCubeColors,
  toFacelet,
  colorFaceletsToCube,

  -- * UDSlice
  numUDSEdges,
  UDSlice,
  UDSlicePermu,
  UDEdgePermu,
  FlipUDSlice,

  -- ** (De)construction
  unsafeUDSlice,
  unsafeUDSlicePermu,
  unsafeUDEdgePermu,
  fromUDSlice,
  fromUDSlicePermu,
  fromUDEdgePermu,

  -- ** Symmetry
  conjugateFlipUDSlice,
  ) where

import Facelet as F
import Misc

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Function ( on )
import Data.Maybe
import Data.List
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
-- In a solvable Rubik's cube (checked separately by @solvable@),
-- its parity must be equal to that of the associated @EdgePermu@.
cornerPermu :: Vector Int -> Maybe CornerPermu
cornerPermu = (CornerPermu <$>) . mfilter check . Just
  where check v = U.length v == numCorners
               && isPermutationVector v

unsafeCornerPermu = CornerPermu

-- | Check that the argument is a vector of senary (6) values of size 8 and
-- wrap it.
--
-- In a solvable Rubik's cube (checked separately by @solvable@),
-- only ternary values are possible;
-- i.e., all elements must be between 0 and 2.
-- Their sum must also be a multiple of 3.
cornerOrien :: Vector Int -> Maybe CornerOrien
cornerOrien = (CornerOrien <$>) . mfilter check . Just
  where check v = U.length v == numCorners
               && U.all (\o -> 0 <= o && o < 6) v

unsafeCornerOrien = CornerOrien

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
-- In a solvable Rubik's cube (checked separately by @solvable@),
-- its parity must be equal to that of the associated @CornerPermu@.
edgePermu :: Vector Int -> Maybe EdgePermu
edgePermu = (EdgePermu <$>) . mfilter check . Just
  where check v = U.length v == numEdges
               && isPermutationVector v

unsafeEdgePermu = EdgePermu

-- | Check that the argument is a vector of binary values of size 12 and
-- wrap it.
--
-- In a solvable Rubik's cube (checked separately by @solvable@),
-- their sum must be even.
edgeOrien :: Vector Int -> Maybe EdgeOrien
edgeOrien = (EdgeOrien <$>) . mfilter check . Just
  where check v = U.length v == numEdges
               && U.all (`elem` [0, 1]) v

unsafeEdgeOrien = EdgeOrien

-- Complete cube

-- | A cube is given by the positions of its corners and edges.
--
-- Cubes are identified with the permutations that produce them starting
-- from the solved cube.
--
-- The cube permutation composition (@class Group Cube@) is defined
-- \"in left to right order\", so that the sequence of movements
-- \"@x@ then @y@ then @z@\" is represented by @x ? y ? z@.
data Cube = Cube
  { corner :: Corner
  , edge   :: Edge }
  deriving (Eq, Show)

class FromCube a where
  fromCube :: Cube -> a

instance (FromCube a, FromCube b) => FromCube (a, b) where
  fromCube c = (fromCube c, fromCube c)

-- | Group action of @Cube@ on type @a@
--
-- >  x `cubeAction` iden == x
-- > (x `cubeAction` a) `cubeAction` b == x `cubeAction (a ? b)
--
-- It seems that with proper additional laws
-- between @FromCube@ and @Group@ instances,
-- it may be possible to automatically deduce a default @CubeAction@ instance.
--
-- > cubeAction a = (a ?) . fromCube
--
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

-- | @numCorners = 8@
numCorners = 8 :: Int

-- | @numEdges = 12@
numEdges = 12 :: Int

-- Orientations are permutations of 3 facelets.
-- They are mapped to integers in @[0 .. 5]@
-- such that @[0, 1, 2]@ are rotations (even permutations)
-- and @[3, 4, 5]@  are transpositions (although impossible in a Rubik's cube).
o `oPlus` o' | o < 3 && o' < 3 =       (o + o')  `mod` 3
             | o < 3           = 3 + ( (o'- o)   `mod` 3)
             |          o' < 3 = 3 + ( (o + o')  `mod` 3)
             | otherwise       =     (-(o + o')) `mod` 3

oInv o | o == 0    = 0
       | o < 3     = 3 - o
       | o == 3    = 3
       | otherwise = 9 - o

--

instance Group CornerPermu where
  iden = CornerPermu $ idVector numCorners
  inverse (CornerPermu a) = CornerPermu $ inverseVector a
  (CornerPermu b) ? (CornerPermu c) = CornerPermu $ composeVector b c

instance Group EdgePermu where
  iden = EdgePermu $ idVector numEdges
  inverse (EdgePermu a) = EdgePermu $ inverseVector a
  (EdgePermu b) ? (EdgePermu c) = EdgePermu $ composeVector b c

instance CubeAction CornerPermu where
  cubeAction cp_ = (cp_ ?) . fromCube

instance CubeAction EdgePermu where
  cubeAction ep_ = (ep_ ?) . fromCube

-- Helper function to define the action of @Cube@ on @CornerOrien@
actionCorner :: CornerOrien -> Corner -> CornerOrien
actionCorner (CornerOrien o) (Corner (CornerPermu gp) (CornerOrien go))
  = CornerOrien $ U.zipWith oPlus (U.backpermute o gp) go

-- Helper function to define the action of @Cube@ on @EdgeOrien@
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

instance Group Corner where
  iden = Corner iden idCornerO
    where idCornerO = CornerOrien $ U.replicate numCorners 0

  inverse (Corner ap_  (CornerOrien ao))
    =      Corner ap_' (CornerOrien ao')
    where ap_'@(CornerPermu ap') = inverse ap_
          ao'                    = U.map oInv . U.backpermute ao $ ap'

  (?)   (Corner bp_ bo_)
      c@(Corner cp_ co_)
    =    Corner dp_ do_
    where dp_ = bp_ ?              cp_
          do_ = bo_ `actionCorner` c

instance Group Edge where
  iden = Edge iden idEdgeO
    where idEdgeO = EdgeOrien $ U.replicate numEdges 0

  inverse (Edge ap_  (EdgeOrien ao))
    =      Edge ap_' (EdgeOrien ao')
    where ap_'@(EdgePermu ap') = inverse ap_
          ao'                  = U.backpermute ao ap'

  (?)   (Edge bp_ bo_)
      c@(Edge cp_ co_)
    =    Edge dp_ do_
    where dp_ = bp_ ?            cp_
          do_ = bo_ `actionEdge` c

--

instance Group Cube where
  iden = Cube iden iden
  inverse (Cube c e) = Cube (inverse c) (inverse e)
  (Cube cA eA) ? (Cube cB eB) = Cube (cA ? cB) (eA ? eB)

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

-- 0 <= o < 6
symRotate :: Int -> [Int] -> [Int]
symRotate o
  | o < 3     = rotate o            -- Even permutation
  | otherwise = rotate (o - 3) . sym -- Odd permutation
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
-- Evaluates to a @Left@ error if a combination of colors does not correspond to
-- a regular cubie from the solved cube: the colors of the facelets on one
-- cubie must be unique, and must not contain facelets of opposite faces.
-- The error is the list of indices of facelets of such an invalid cubie.
--
-- Another possible error is that the resulting configuration is not a
-- permutation of cubies (at least one cubie is absent, and one is duplicated).
-- In that case, the result is @Right Nothing@.
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
    -- > symRotate o x = xs !! i
    -- An error is returned otherwise
    findPos :: [[Int]] -> [Int] -> [Int] -> e -> Either e (Int, Int)
    findPos xs os x e
      = case join . find isJust $
          map
            (\o -> do
              i <- elemIndex (symRotate o x) xs
              Just (o, i))
            os
        of
          Nothing -> Left e
          Just x -> Right x

stringOfCubeColors :: Cube -> String
stringOfCubeColors =  stringOfColorFacelets' . toFacelet

--

-- ** UDSlice

-- | Position of the 4 UDSlice edges up to permutation (carried-to).
-- The vector is always sorted.
newtype UDSlice = UDSlice { fromUDSlice :: Vector Int }
  deriving (Eq, Show)

-- | Position of the 4 UDSlice edges (replaced-by),
-- assuming they are all in that slice already.
newtype UDSlicePermu = UDSlicePermu { fromUDSlicePermu :: Vector Int }
  deriving (Eq, Show)

-- | Position of the 8 other edges (replaced-by),
-- assuming UDSlice edges are in that slice already.
newtype UDEdgePermu = UDEdgePermu { fromUDEdgePermu :: Vector Int }
  deriving (Eq, Show)

type FlipUDSlice = (EdgeOrien, UDSlice)

-- | Wrap an increasing list of 4 elements in @[0 .. 11]@.
unsafeUDSlice = UDSlice

-- | Wrap a permutation of size 4.
unsafeUDSlicePermu = UDSlicePermu

-- | Wrap a permutation of size 8.
unsafeUDEdgePermu = UDEdgePermu

-- | > numUDSEdges = 4
numUDSEdges = 4 :: Int

vSort = U.fromList . sort . U.toList

-- Projections of the identity cube
neutralUDSlice = UDSlice $ U.enumFromN 8 numUDSEdges -- 4
neutralUDSlicePermu = UDSlicePermu $ U.enumFromN 0 numUDSEdges -- 4
neutralUDEdgePermu = UDEdgePermu $ U.enumFromN 0 (numEdges - numUDSEdges) -- 8

actionUDSlice :: UDSlice -> EdgePermu -> UDSlice
actionUDSlice (UDSlice s) (EdgePermu ep) = UDSlice (act s)
  where
    act = vSort . U.map (fromJust . flip U.elemIndex ep)

-- EdgePermu should leave UDSlice stable.
actionUDSlicePermu :: UDSlicePermu -> EdgePermu -> UDSlicePermu
actionUDSlicePermu (UDSlicePermu sp) (EdgePermu ep) =
  UDSlicePermu $ sp `composeVector` U.map (subtract 8) (U.drop 8 ep)

-- EdgePermu should leave UDSlice stable.
actionUDEdgePermu :: UDEdgePermu -> EdgePermu -> UDEdgePermu
actionUDEdgePermu (UDEdgePermu ep') (EdgePermu ep) =
  UDEdgePermu $ ep' `composeVector` U.take 8 ep

instance CubeAction UDSlice where
  cubeAction s = actionUDSlice s . fromCube

instance CubeAction UDSlicePermu where
  cubeAction sp = actionUDSlicePermu sp . fromCube

instance CubeAction UDEdgePermu where
  cubeAction e = actionUDEdgePermu e . fromCube

instance FromCube UDSlice where
  fromCube = cubeAction neutralUDSlice

instance FromCube UDSlicePermu where
  fromCube = cubeAction neutralUDSlicePermu

instance FromCube UDEdgePermu where
  fromCube = cubeAction neutralUDEdgePermu

-- TODO: Make a type class of this (?)
-- | The conjugation is only compatible when the @Cube@ symmetry
-- leaves UDSlice edges stable, and either flips them all or none of them,
-- and either flips all 8 non-UDSlice edges or none of them.
conjugateFlipUDSlice :: Cube -> FlipUDSlice -> FlipUDSlice
conjugateFlipUDSlice c = assert conjugable conjugate
  where
    (EdgeOrien eo_c, EdgePermu ep_c) = fromCube c
    conjugable
      = let fromCube_c = UDSlice . vSort . U.drop 8 $ ep_c
        in fromCube_c == neutralUDSlice
           && isConstant (U.take 8 $ eo_c)
           && isConstant (U.drop 8 $ eo_c)
    isConstant v = U.init v == U.tail v
    udsO = eo_c U.! 8
    altO = eo_c U.! 0
    conjugate (EdgeOrien eo, UDSlice u) = (EdgeOrien eo', _u')
      where
        eo' = U.zipWith
                (\o p -> (o + eo U.! p + bool altO udsO (p `U.elem` u)) `mod` 2)
                (eo_c)
                (ep_c)
        _u' = cubeAction (UDSlice u) c

