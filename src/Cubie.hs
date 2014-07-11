{- |
   Cubie representation.

   A Rubik's cube is the cartesian product of a permutation of cubies
   and an action on their orientations.
-}

module Cubie (
  -- * Complete cube
  Cube (..),
  CubeAction (..),
  mkCube,

  -- * Corners
  numCorners,
  CornerPermu (..),
  CornerOrien (..),
  CornerCubie (..),
  
  -- * Edges
  numEdges,
  EdgePermu (..),
  EdgeOrien (..),
  EdgeCubie (..),

  -- * Projections
  cornerP,
  cornerO,
  edgeP,
  edgeO,

  -- * Conversions
  printCube,
  toFacelet,
  fromColorFacelets,

  -- * UDSlice
  numUDSEdges,
  UDSlice (..),
  UDSlicePermu (..),
  UDEdgePermu (..),

  conjugateFlipUDSlice,

  fromCube,

  -- * Facelets corresponding to each cubie
  -- $mnemonic 

  -- ** Corners
  cornerFacelets,
  ulb, ufl, urf, ubr, dlf, dfr, drb, dbl,

  -- ** Edges
  edgeFacelets,
  ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br
  ) where

import Facelet as F
import Misc

import Control.Monad

import Data.Function ( on )
import Data.Maybe
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

-- Facelets corresponding to each cubie

-- $mnemonic
-- The first letter in the name of a cubie is
-- the color of its reference facelet
-- (see <http://kociemba.org/math/cubielevel.htm>).
--
-- Corner colors are given in clockwise order
--
-- Corners are lexicographically ordered
-- (@U>L>F>R>B>D@).
-- 
-- Edges are gathered by horizontal slices (@U, D, UD@).
--

ulb = [ 0,  9, 38]
ufl = [ 6, 18, 11]
urf = [ 8, 27, 20]
ubr = [ 2, 36, 29]
dlf = [45, 17, 24]
dfr = [47, 26, 33]
drb = [53, 35, 42]
dbl = [51, 44, 15]

cornerFacelets :: [[Int]]
cornerFacelets = [ulb, ufl, urf, ubr, dlf, dfr, drb, dbl]

ul = [ 3, 10]
uf = [ 7, 19]
ur = [ 5, 28]
ub = [ 1, 37]
dl = [48, 16]
df = [46, 25]
dr = [50, 34]
db = [52, 43]
fl = [21, 14]
fr = [23, 30]
bl = [41, 12]
br = [39, 32]

-- | 
-- > edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]
edgeFacelets :: [[Int]]
edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]

--

-- | Cubie permutation is in replaced-by representation.
newtype CornerPermu = CornerPermu (Vector Int)
  deriving (Eq, Show)
newtype CornerOrien = CornerOrien (Vector Int)
  deriving (Eq, Show)
data CornerCubie = Corner
  { cPermu :: CornerPermu
  , cOrien :: CornerOrien }
  deriving (Eq, Show)

--

-- | Cubie permutation is in replaced-by representation.
newtype EdgePermu = EdgePermu (Vector Int)
  deriving (Eq, Show)
newtype EdgeOrien = EdgeOrien (Vector Int)
  deriving (Eq, Show)
data EdgeCubie = Edge
  { ePermu :: EdgePermu
  , eOrien :: EdgeOrien }
  deriving (Eq, Show)

-- Complete cube

-- | A cube is given by the positions of
-- its corners and edges.
data Cube = Cube
  { corner :: CornerCubie
  , edge   :: EdgeCubie }
  deriving (Eq, Show)

-- | Group action of @Cube@ on type @a@
--
-- >  x `cubeAction` iden == x
-- > (x `cubeAction` a) `cubeAction` b == x `cubeAction (a ? b)
--
class CubeAction a where
  cubeAction :: a -> Cube -> a

instance (CubeAction a, CubeAction b) => CubeAction (a, b) where
  cubeAction (x, y) c = (cubeAction x c, cubeAction y c)

mkCube :: Vector Int -> Vector Int -> Vector Int -> Vector Int -> Cube
mkCube cp co ep eo = Cube c e
  where c = Corner (CornerPermu cp) (CornerOrien co)
        e = Edge (EdgePermu ep) (EdgeOrien eo)

--

cornerP :: Cube -> CornerPermu
cornerP = cPermu . corner

cornerO :: Cube -> CornerOrien
cornerO = cOrien . corner

edgeP :: Cube -> EdgePermu
edgeP = ePermu . edge

edgeO :: Cube -> EdgeOrien
edgeO = eOrien . edge

--

-- | @numCorners == 8@
numCorners = 8 :: Int

-- | @numEdges == 12@
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
  cubeAction cp_ = (cp_ ?) . cPermu . corner

instance CubeAction EdgePermu where
  cubeAction ep_ = (ep_ ?) . ePermu . edge

-- Helper function to define the action of @Cube@ on @CornerOrien@
actionCorner :: CornerOrien -> CornerCubie -> CornerOrien
actionCorner (CornerOrien o) (Corner (CornerPermu gp) (CornerOrien go))
  = CornerOrien $ U.zipWith oPlus (U.backpermute o gp) go

-- Helper function to define the action of @Cube@ on @EdgeOrien@
actionEdge :: EdgeOrien -> EdgeCubie -> EdgeOrien
actionEdge (EdgeOrien o) (Edge (EdgePermu gp) (EdgeOrien go))
  = EdgeOrien $ U.zipWith (((`mod` 2) .) . (+)) (U.backpermute o gp) go

instance CubeAction CornerOrien where
  cubeAction co_ = actionCorner co_ . corner

instance CubeAction EdgeOrien where
  cubeAction eo_ = actionEdge eo_ . edge

--

instance Group CornerCubie where
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

instance Group EdgeCubie where
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
  = Facelets $ U.create (do
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

fromColorFacelets :: F.ColorFacelets -> Cube
fromColorFacelets (F.ColorFacelets cc) = mkCube cp co ep eo
  where
    (co, cp) = U.unzip . U.fromList . map findCorner
               . colorsOf $ cornerFacelets
    (eo, ep) = U.unzip . U.fromList . map findEdge
               . colorsOf $ edgeFacelets
    colorsOf = map $ map (cc U.!)
    findCorner = findPos cornerColors [0 .. 5]
    findEdge   = findPos edgeColors [0, 1]
    cornerColors = map (map F.color) cornerFacelets
    edgeColors = map (map F.color) edgeFacelets
    -- Find the orientation and index (o, i)
    -- o <- os, 0 <= i < length xs
    -- such that the oriented color pattern (symRotate o x)
    -- matches with the i-th in xs.
    -- Compatible with both corner and edge orientations
    findPos :: [[Int]] -> [Int] -> [Int] -> (Int, Int)
    findPos xs os x
      = head $ mapMaybe
          (\o -> fmap ((,) o) $ elemIndex (symRotate o x) xs)
          os

printCube :: Cube -> IO ()
printCube = F.printColor . toFacelet

--

-- ** UDSlice

-- | Position of the 4 UDSlice edges up to permutation (carried-to)
-- The vector is always sorted.
newtype UDSlice = UDSlice (Vector Int)
  deriving (Eq, Show)
-- | Position of the 4 UDSlice edges,
-- assuming they are all in that slice already.
newtype UDSlicePermu = UDSlicePermu (Vector Int)
-- | Position of the 8 other edges,
-- assuming UDSlice edges are in that slice already.
newtype UDEdgePermu = UDEdgePermu (Vector Int)

numUDSEdges = 4 :: Int

vSort = U.fromList . sort . U.toList

-- Projections of the identity cube
neutralUDSlice = UDSlice $ U.enumFromN 8 numUDSEdges -- 4
neutralUDSlicePermu = UDSlicePermu $ U.enumFromN 0 numUDSEdges -- 4
neutralUDEdgePermu = UDEdgePermu $ U.enumFromN 0 (numEdges - numUDSEdges) -- 8

actionUDSlice :: UDSlice -> EdgePermu -> UDSlice
actionUDSlice (UDSlice s) (EdgePermu ep) = UDSlice s'
  where
    s' = vSort
         $ U.map (fromJust . flip U.elemIndex ep) s

-- EdgePermu should leave UDSlice stable.
actionUDSlicePermu :: UDSlicePermu -> EdgePermu -> UDSlicePermu
actionUDSlicePermu (UDSlicePermu sp) (EdgePermu ep) =
  UDSlicePermu $ sp `composeVector` U.map (subtract 8) (U.drop 8 ep)

-- EdgePermu should leave UDSlice stable.
actionUDEdgePermu :: UDEdgePermu -> EdgePermu -> UDEdgePermu
actionUDEdgePermu (UDEdgePermu ep') (EdgePermu ep) =
  UDEdgePermu $ ep' `composeVector` U.take 8 ep

instance CubeAction UDSlice where
  cubeAction s = actionUDSlice s . edgeP

instance CubeAction UDSlicePermu where
  cubeAction sp = actionUDSlicePermu sp . edgeP

instance CubeAction UDEdgePermu where
  cubeAction e = actionUDEdgePermu e . edgeP

edgePermuToUDSlice :: EdgePermu -> UDSlice
edgePermuToUDSlice = actionUDSlice neutralUDSlice

edgePermuToUDSlicePermu :: EdgePermu -> UDSlicePermu
edgePermuToUDSlicePermu = actionUDSlicePermu neutralUDSlicePermu

edgePermuToUDEdgePermu :: EdgePermu -> UDEdgePermu
edgePermuToUDEdgePermu = actionUDEdgePermu neutralUDEdgePermu

-- TODO: Make a type class of this (?)
-- | The conjugation is only compatible when the @Cube@ symmetry
-- leaves UDSlice edges stable, and either flips them all or none of them,
-- and either flips all 8 non-UDSlice edges or none of them.
conjugateFlipUDSlice :: Cube -> (EdgeOrien, UDSlice) -> (EdgeOrien, UDSlice)
conjugateFlipUDSlice c | conjugable = conjugate
  where
    EdgeOrien eo_c = edgeO c
    EdgePermu ep_c = edgeP c
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

-- TODO: Make a type class of this
fromCube c = UDSlice . vSort . U.drop 8 $ ep
  where EdgePermu ep = edgeP c
