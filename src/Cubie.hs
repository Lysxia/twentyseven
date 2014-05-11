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
  -- $udslice
  UDSlice (..),
  UDSlicePermu (..),
  UDEdgePermu (..),

  -- * Facelets corresponding to each cubie
  cornerFacelets,
  ulb, ufl, urf, ubr, dlf, dfr, drb, dbl,
  edgeFacelets,
  ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br
  ) where

import Data.Maybe
import Data.Array.Unboxed
import Data.List
import Misc
import Facelet as F

-- * Facelets corresponding to each cubie

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

edgeFacelets :: [[Int]]
edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]

--

newtype CornerPermu = CornerPermu [Int]
newtype CornerOrien = CornerOrien [Int]
data CornerCubie = Corner
  { cPermu :: CornerPermu
  , cOrien :: CornerOrien }

--

newtype EdgePermu = EdgePermu [Int]
newtype EdgeOrien = EdgeOrien [Int]
data EdgeCubie = Edge
  { ePermu :: EdgePermu
  , eOrien :: EdgeOrien }

-- Complete cube

-- | A cube is given by the positions of
-- its corners and edges.
data Cube = Cube
  { corner :: CornerCubie
  , edge   :: EdgeCubie }

-- | Group action of @Cube@ on type @a@
--
-- >  x `cubeAction` iden == x
-- > (x `cubeAction` a) `cubeAction` b == x `cubeAction (a ? b)
--
class CubeAction a where
  cubeAction :: a -> Cube -> a

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
numCorners :: Int
numCorners = 8

-- | @numEdges == 12@
numEdges :: Int
numEdges = 12

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
  iden = CornerPermu [0 .. numCorners - 1]
  inverse (CornerPermu a) = CornerPermu $ inverseList numCorners a
  (CornerPermu b) ? (CornerPermu c) = CornerPermu $ b `composeList` c

instance Group EdgePermu where
  iden = EdgePermu [0 .. numEdges - 1]
  inverse (EdgePermu a) = EdgePermu $ inverseList numEdges a
  (EdgePermu b) ? (EdgePermu c) = EdgePermu $ b `composeList` c

instance CubeAction CornerPermu where
  cubeAction cp_ = (cp_ ?) . cPermu . corner

instance CubeAction EdgePermu where
  cubeAction ep_ = (ep_ ?) . ePermu . edge

-- Helper function to define the action of @Cube@ on @CornerOrien@
actionCorner :: CornerOrien -> CornerCubie -> CornerOrien
actionCorner (CornerOrien o) (Corner (CornerPermu gp) (CornerOrien go))
  = CornerOrien $ zipWith (oPlus.(o !!)) gp go

-- Helper function to define the action of @Cube@ on @EdgeOrien@
actionEdge :: EdgeOrien -> EdgeCubie -> EdgeOrien
actionEdge (EdgeOrien o) (Edge (EdgePermu gp) (EdgeOrien go))
  = EdgeOrien $ zipWith (((`mod` 2) .).(+).(o !!)) gp go

instance CubeAction CornerOrien where
  cubeAction co_ = actionCorner co_ . corner

instance CubeAction EdgeOrien where
  cubeAction eo_ = actionEdge eo_ . edge

--

instance Group CornerCubie where
  iden = Corner iden idCornerO
    where idCornerO = CornerOrien $ replicate numCorners 0

  inverse (Corner ap_  (CornerOrien ao))
    =      Corner ap_' (CornerOrien ao')
    where ap_'@(CornerPermu ap') = inverse ap_
          ao'                    = map (oInv . (ao !!)) ap'

  (?)   (Corner bp_ bo_)
      c@(Corner cp_ co_)
    =    Corner dp_ do_
    where dp_ = bp_ ?              cp_
          do_ = bo_ `actionCorner` c

instance Group EdgeCubie where
  iden = Edge iden idEdgeO
    where idEdgeO = EdgeOrien $ replicate numEdges 0

  inverse (Edge ap_  (EdgeOrien ao))
    =      Edge ap_' (EdgeOrien ao')
    where ap_'@(EdgePermu ap') = inverse ap_
          ao'                  = map (ao !!) ap'

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

-- o < 6
symRotate :: Int -> [Int] -> [Int]
symRotate o | o < 3     = rotate o
            | otherwise = rotate (o - 3) . sym
  where sym [a,b,c] = [a,c,b]

toFacelet :: Cube -> Facelets
toFacelet
  (Cube
    { corner = Corner (CornerPermu cp) (CornerOrien co)
    , edge   = Edge (EdgePermu ep) (EdgeOrien eo) })
  = Facelets $ array F.boundsF $ corners ++ edges ++ centers
  where setFacelets p o f = zip (concat f)
                              $ concat $ zipWith symRotate o $ map (f !!) p
        corners = setFacelets cp co cornerFacelets
        edges   = setFacelets ep eo edgeFacelets
        centers = [(x,x) | x <- [4, 13 .. 49]]

fromColorFacelets :: F.ColorFacelets -> Cube
fromColorFacelets (F.ColorFacelets cc) = mkCube cp co ep eo
  where (cp, co) = unzip $ map (pAndO cornerColors 0 . map (cc !))
                               cornerFacelets
        (ep, eo) = unzip $ map (pAndO edgeColors 0 . map (cc !)) edgeFacelets
        cornerColors = map (map F.color) cornerFacelets
        edgeColors = map (map F.color) edgeFacelets
        pAndO l o colors | o < 6     = case elemIndex (symRotate o colors) l of
                                         Nothing -> pAndO l (o+1) colors
                                         Just i  -> (i, o)
                         | otherwise = undefined

printCube :: Cube -> IO ()
printCube = F.printColor . toFacelet

--


-- ** UDSlice
-- $udslice
-- Positions of the 4 UDSlice edges,
-- given in ascending order,
-- i.e., position up to permutation of the 4 edges
-- (carried-to)

newtype UDSlice = UDSlice [Int]
newtype UDSlicePermu = UDSlicePermu [Int]
newtype UDEdgePermu = UDEdgePermu [Int]

neutralUDSlice = UDSlice [0..3]
neutralUDSlicePermu = UDSlicePermu [0..3]
neutralUDEdgePermu = UDEdgePermu [0..7]

actionUDSlice :: UDSlice -> EdgePermu -> UDSlice
actionUDSlice (UDSlice s) (EdgePermu ep) = UDSlice s'
  where s' = sort $ map (subtract 8 . fromJust . flip elemIndex ep . (+ 8)) s

-- EdgePermu should leave USlice in place
actionUDSlicePermu :: UDSlicePermu -> EdgePermu -> UDSlicePermu
actionUDSlicePermu (UDSlicePermu sp) (EdgePermu ep) =
  UDSlicePermu $ sp `composeList` map (($ 8) . (-)) (drop 8 ep)

-- Same comment as above
actionUDEdgePermu :: UDEdgePermu -> EdgePermu -> UDEdgePermu
actionUDEdgePermu (UDEdgePermu ep') (EdgePermu ep) =
  UDEdgePermu $ ep' `composeList` take 8 ep

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

