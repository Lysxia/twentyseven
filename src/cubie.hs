module Cubie where

import Data.Array.Unboxed
import Data.List
import qualified Facelet as F
import Misc

-- Cubie representation, replaced-by representation

newtype CornerPermu = CornerPermu [Int]
newtype CornerOrien = CornerOrien [Int]
newtype CornerCubie = CornerCubie (CornerPermu, CornerOrien)

newtype EdgePermu = EdgePermu [Int]
newtype EdgeOrien = EdgeOrien [Int]
newtype EdgeCubie = EdgeCubie (EdgePermu, EdgeOrien)

data Cube =
  Cube { cornerP :: CornerPermu,
         cornerO :: CornerOrien,
         edgeP   :: EdgePermu,
         edgeO   :: EdgeOrien
       }

cube_ cp_ co_ ep_ eo_ =
  Cube { cornerP = cp_, cornerO = co_, edgeP = ep_, edgeO = eo_ }

cube cp co ep eo = cube_ cp_ co_ ep_ eo_
  where cp_ = CornerPermu cp
        co_ = CornerOrien co
        ep_ = EdgePermu ep
        eo_ = EdgeOrien eo

cubie cp_ co_ ep_ eo_ = (CornerCubie (cp_, co_), EdgeCubie (ep_, eo_))

numCorners :: Int
numCorners = 8

numEdges :: Int
numEdges = 12

idCornerO = CornerOrien $ replicate numCorners 0
idEdgeO = EdgeOrien $ replicate numEdges 0

o `oPlus` o' | o < 3 && o' < 3 = (o + o') `mod` 3
             | o < 3           = 3 + ((o' - o) `mod` 3)
             |          o' < 3 = 3 + ((o + o') `mod` 3)
             | otherwise       = (-(o + o')) `mod` 3

instance Group CornerPermu where
  iden = CornerPermu [0..numCorners-1]
  (CornerPermu a) `compose` (CornerPermu b) = CornerPermu $ map (a !!) b

instance Group CornerCubie where
  iden = CornerCubie (iden, idCornerO)
  compose (CornerCubie (ap_, CornerOrien ao))
          (CornerCubie (bp_@(CornerPermu bp), CornerOrien bo)) =
    CornerCubie (ap_ `compose` bp_, CornerOrien o)
    where o = zipWith (oPlus.(ao !!)) bp bo

instance Group EdgePermu where
  iden = EdgePermu [0..numEdges-1]
  (EdgePermu a) `compose` (EdgePermu b) = EdgePermu $ map (a !!) b

instance Group EdgeCubie where
  iden = EdgeCubie (iden, idEdgeO)
  compose (EdgeCubie (ap_, EdgeOrien ao))
          (EdgeCubie (bp_@(EdgePermu bp), EdgeOrien bo)) =
    EdgeCubie (ap_ `compose` bp_, EdgeOrien o)
    where o = zipWith (((`mod` 2) .).(+).(ao !!)) bp bo

instance Group Cube where
  iden = cube_ idcp_ idco_ idep_ ideo_
    where CornerCubie (idcp_, idco_) = iden
          EdgeCubie   (idep_, ideo_) = iden
  compose (Cube { cornerP = cp1, cornerO = co1, edgeP = ep1, edgeO = eo1 })
          (Cube { cornerP = cp2, cornerO = co2, edgeP = ep2, edgeO = eo2 }) =
    cube_ cp_ co_ ep_ eo_
    where (CornerCubie (cp_, co_), EdgeCubie (ep_, eo_)) =
            (cubie cp1 co1 ep1 eo1) `compose` (cubie cp2 co2 ep2 eo2)

-- Facelets corresponding to each cubie

ulb = [ 1, 10, 39]
ufl = [ 7, 19, 12]
urf = [ 9, 28, 21]
ubr = [ 3, 37, 30]
dlf = [46, 18, 25]
dfr = [48, 27, 34]
drb = [54, 36, 43]
dbl = [52, 45, 16]

cornerFacelets = [ulb, ufl, urf, ubr, dlf, dfr, drb, dbl]

ul = [ 4, 11]
uf = [ 8, 20]
ur = [ 6, 29]
ub = [ 2, 38]
dl = [49, 17]
df = [47, 26]
dr = [51, 35]
db = [53, 44]
fl = [22, 15]
fr = [24, 31]
bl = [42, 13]
br = [40, 33]

edgeFacelets = [ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br]

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
fromColorCube (F.ColorCube cc) = cube cp co ep eo
  where (cp, co) = unzip $ map (pAndO cornerColors 0 . map (cc !))
                               cornerFacelets
        (ep, eo) = unzip $ map (pAndO edgeColors 0 . map (cc !)) edgeFacelets
        cornerColors = map (map F.color) cornerFacelets
        edgeColors = map (map F.color) edgeFacelets
        pAndO l o colors | o < 6     = case elemIndex (symRotate o colors) l of
                                         Nothing -> pAndO l (o+1) colors
                                         Just i  -> (i, o)
                         | otherwise = undefined

-- Elementary moves

uCubie =
  cube ([1, 2, 3, 0] ++ [4..7])
       (replicate 8 0)
       ([1, 2, 3, 0] ++ [4..11])
       (replicate 12 0)

surf3Cubie =
  cube [4, 5, 2, 1, 6, 3, 0, 7]
       [2, 1, 2, 1, 2, 1, 2, 1]
       [5, 9, 1, 8, 7, 11, 3, 10, 6, 2, 4, 0]
       [1, 0, 1, 0, 1,  0, 1,  0, 1, 1, 1, 1]

sf2Cubie =
  cube [6, 5, 4, 7, 2, 1, 0, 3]
       (replicate 8 0)
       [6, 5, 4, 7, 2, 1, 0, 3, 9, 8, 11, 10]
       (replicate 12 0)

su4Cubie =
  cube [1, 2, 3, 0, 5, 6, 7, 4]
       (replicate 8 0)
       [1, 2, 3, 0, 5, 6, 7, 4, 9, 11, 8, 10]
       (replicate 8 0 ++ [1, 1, 1, 1])

slr2Cubie =
  cube [3, 2, 1, 0, 5, 4, 7, 6]
       (replicate 8 3)
       [2, 1, 0, 3, 6, 5, 4, 7, 9, 8, 11, 10]
       (replicate 12 0)
