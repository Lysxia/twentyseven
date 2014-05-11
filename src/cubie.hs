module Cubie where

import Data.Maybe
import Data.Array.Unboxed
import Data.List
import Misc
import qualified Facelet as F

-- Facelets corresponding to each cubie

ulb = [ 0,  9, 38]
ufl = [ 6, 18, 11]
urf = [ 8, 27, 20]
ubr = [ 2, 36, 29]
dlf = [45, 17, 24]
dfr = [47, 26, 33]
drb = [53, 35, 42]
dbl = [51, 44, 15]

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

edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]

-- Cubie representation, replaced-by representation

newtype CornerPermu = CornerPermu [Int]
newtype CornerOrien = CornerOrien [Int]
newtype CornerCubie = CornerCubie (CornerPermu, CornerOrien)

newtype EdgePermu = EdgePermu [Int]
newtype EdgeOrien = EdgeOrien [Int]
newtype EdgeCubie = EdgeCubie (EdgePermu, EdgeOrien)

newtype UDSlice = UDSlice [Int] -- Positions of the 4 UDSlice edges,
                                -- given in ascending order
                                -- => position up to permutation of the 4 edges

newtype UDSlicePermu = UDSlicePermu [Int]
newtype UDEdgePermu = UDEdgePermu [Int]

data Cube =
  Cube { cornerP :: CornerPermu,
         cornerO :: CornerOrien,
         edgeP   :: EdgePermu,
         edgeO   :: EdgeOrien
       }

class CubeAction a where
  cubeAction :: a -> Cube -> a

mkCube cp co ep eo = Cube cp_ co_ ep_ eo_
  where cp_ = CornerPermu cp
        co_ = CornerOrien co
        ep_ = EdgePermu ep
        eo_ = EdgeOrien eo

cubie cp_ co_ ep_ eo_ = (CornerCubie (cp_, co_), EdgeCubie (ep_, eo_))

--

printCube :: Cube -> IO ()
printCube = F.printColor . toFacelet

--

cubeToCorner :: Cube -> CornerCubie
cubeToCorner (Cube { cornerP = cp_, cornerO = co_ }) =
  CornerCubie (cp_, co_)

cubeToEdge :: Cube -> EdgeCubie
cubeToEdge (Cube { edgeP = ep_, edgeO = eo_ }) =
  EdgeCubie (ep_, eo_)

--

numCorners :: Int
numCorners = 8

numEdges :: Int
numEdges = 12

o `oPlus` o' | o < 3 && o' < 3 =       (o + o') `mod` 3
             | o < 3           = 3 + ( (o' - o)  `mod` 3)
             |          o' < 3 = 3 + ( (o + o')  `mod` 3)
             | otherwise       =     (-(o + o')) `mod` 3

oInv o | o == 0    = 0
       | o < 3     = 3 - o
       | o == 3    = 3
       | otherwise = 9 - o

--

instance Group CornerPermu where
  iden = CornerPermu [0..numCorners-1]
  inverse (CornerPermu a) = CornerPermu $ inverseList numCorners a
  (CornerPermu b) ? (CornerPermu c) = CornerPermu $ b `composeList` c

instance Group EdgePermu where
  iden = EdgePermu [0..numEdges-1]
  inverse (EdgePermu a) = EdgePermu $ inverseList numEdges a
  (EdgePermu b) ? (EdgePermu c) = EdgePermu $ b `composeList` c

instance CubeAction CornerPermu where
  cubeAction cp_ = (cp_ ?) . cornerP

instance CubeAction EdgePermu where
  cubeAction ep_ = (ep_ ?) . edgeP

actionCorner :: CornerOrien -> CornerCubie -> CornerOrien
actionCorner (CornerOrien o) (CornerCubie (CornerPermu gp, CornerOrien go)) =
  CornerOrien $ zipWith (oPlus.(o !!)) gp go

actionEdge :: EdgeOrien -> EdgeCubie -> EdgeOrien
actionEdge (EdgeOrien o) (EdgeCubie (EdgePermu gp, EdgeOrien go)) =
  EdgeOrien $ zipWith (((`mod` 2) .).(+).(o !!)) gp go

instance CubeAction CornerOrien where
  cubeAction co_ = actionCorner co_ . cubeToCorner

instance CubeAction EdgeOrien where
  cubeAction eo_ = actionEdge eo_ . cubeToEdge

--

instance Group CornerCubie where
  iden = CornerCubie (iden, idCornerO)
    where idCornerO = CornerOrien $ replicate numCorners 0

  inverse (CornerCubie (ap_, CornerOrien ao)) = CornerCubie (ap_', CornerOrien ao')
    where ap_'@(CornerPermu ap') = inverse ap_
          ao'                    = map (oInv . (ao !!)) ap'

  (?)   (CornerCubie (bp_, bo_))
      c@(CornerCubie (cp_, co_))
    =    CornerCubie (dp_, do_)
    where dp_ = bp_ ?              cp_
          do_ = bo_ `actionCorner` c

instance Group EdgeCubie where
  iden = EdgeCubie (iden, idEdgeO)
    where idEdgeO = EdgeOrien $ replicate numEdges 0

  inverse (EdgeCubie (ap_, EdgeOrien ao)) = EdgeCubie (ap_', EdgeOrien ao')
    where ap_'@(EdgePermu ap') = inverse ap_
          ao'                  = map (ao !!) ap'

  (?)   (EdgeCubie (bp_, bo_))
      c@(EdgeCubie (cp_, co_))
    =    EdgeCubie (dp_, do_)
    where dp_ = bp_ ?            cp_
          do_ = bo_ `actionEdge` c

--

instance Group Cube where
  iden = Cube idcp_ idco_ idep_ ideo_
    where CornerCubie (idcp_, idco_) = iden
          EdgeCubie   (idep_, ideo_) = iden
  inverse a = Cube cp_ co_ ep_ eo_
    where CornerCubie (cp_, co_) = inverse $ cubeToCorner a
          EdgeCubie   (ep_, eo_) = inverse $ cubeToEdge   a
  b ? c = Cube cp_ co_ ep_ eo_
    where CornerCubie (cp_, co_) = cubeToCorner b ? cubeToCorner c
          EdgeCubie   (ep_, eo_) = cubeToEdge   b ? cubeToEdge   c

-- UDSlice

neutralUDSlice = UDSlice [0..3]
neutralUDSlicePermu = UDSlicePermu [0..3]
neutralUDEdgePermu = UDEdgePermu [0..7]

actionUDSlice :: UDSlice -> EdgePermu -> UDSlice
actionUDSlice (UDSlice s) (EdgePermu ep) = UDSlice s'
  where s' = sort $ map ((flip (-) 8) . fromJust . flip elemIndex ep . (+ 8)) s

-- EdgePermu should leave USlice in place
actionUDSlicePermu :: UDSlicePermu -> EdgePermu -> UDSlicePermu
actionUDSlicePermu (UDSlicePermu sp) (EdgePermu ep) =
  UDSlicePermu $ sp `composeList` map (($ 8).(-)) (drop 8 ep)

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

-- Conversion

-- Facelet conversion

-- o < 6
symRotate :: Int -> [Int] -> [Int]
symRotate o | o < 3     = rotate o
            | otherwise = rotate (o - 3) . sym
  where sym [a,b,c] = [a,c,b]

toFacelet :: Cube -> F.Cube
toFacelet (Cube { cornerP = CornerPermu cp,
                  cornerO = CornerOrien co,
                  edgeP   = EdgePermu ep,
                  edgeO   = EdgeOrien eo }) =
  F.Cube $ array F.boundsF $ corners ++ edges ++ centers
  where setFacelets p o f = zip (concat f)
                              $ concat $ zipWith symRotate o $ map (f !!) p
        corners = setFacelets cp co cornerFacelets
        edges   = setFacelets ep eo edgeFacelets
        centers = [(x,x) | x <- [5,14..50]]

fromColorCube :: F.ColorCube -> Cube
fromColorCube (F.ColorCube cc) = mkCube cp co ep eo
  where (cp, co) = unzip $ map (pAndO cornerColors 0 . map (cc !))
                               cornerFacelets
        (ep, eo) = unzip $ map (pAndO edgeColors 0 . map (cc !)) edgeFacelets
        cornerColors = map (map F.color) cornerFacelets
        edgeColors = map (map F.color) edgeFacelets
        pAndO l o colors | o < 6     = case elemIndex (symRotate o colors) l of
                                         Nothing -> pAndO l (o+1) colors
                                         Just i  -> (i, o)
                         | otherwise = undefined
