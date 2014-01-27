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

cube cp co ep eo = Cube { cornerP = cp, cornerO = co, edgeP = ep, edgeO = eo }
cubie cp co ep eo = (CornerCubie (cp, co), EdgeCubie (ep, eo))

numCorners :: Int
numCorners = 8

numEdges :: Int
numEdges = 12

idCornerO = CornerOrien $ replicate numCorners 0
idEdgeO = EdgeOrien $ replicate numEdges 0

instance Group CornerPermu where
  iden = CornerPermu [0..numCorners-1]
  (CornerPermu a) `compose` (CornerPermu b) = CornerPermu $ map (a !!) b

instance Group CornerCubie where
  iden = CornerCubie (iden, idCornerO)
  compose (CornerCubie (a@(CornerPermu ap), CornerOrien ao))
          (CornerCubie (b@(CornerPermu bp), CornerOrien bo)) =
    CornerCubie (a `compose` b, CornerOrien o)
    where o = zipWith ((+).(ao !!)) bp bo

instance Group EdgePermu where
  iden = EdgePermu [0..numEdges-1]
  (EdgePermu a) `compose` (EdgePermu b) = EdgePermu $ map (a !!) b

instance Group EdgeCubie where
  iden = EdgeCubie (iden, idEdgeO)
  compose (EdgeCubie (a@(EdgePermu ap), EdgeOrien ao))
          (EdgeCubie (b@(EdgePermu bp), EdgeOrien bo)) =
    EdgeCubie (a `compose` b, EdgeOrien o)
    where o = zipWith ((+).(ao !!)) bp bo

instance Group Cube where
  iden = cube idCornerP idCornerO idEdgeP idEdgeO
    where CornerCubie (idCornerP, idCornerO) = iden
          EdgeCubie (idEdgeP, idEdgeO)       = iden
  compose (Cube { cornerP = cp1, cornerO = co1, edgeP = ep1, edgeO = eo1 })
          (Cube { cornerP = cp2, cornerO = co2, edgeP = ep2, edgeO = eo2 }) =
    cube cp co ep eo
    where (CornerCubie (cp, co), EdgeCubie (ep, eo)) =
            compose (cubie cp1 co1 ep1 eo1) (cubie cp2 co2 ep2 eo2)

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

toFacelet :: Cube -> F.Cube
toFacelet (Cube { cornerP = CornerPermu cp,
                  cornerO = CornerOrien co,
                  edgeP   = EdgePermu ep,
                  edgeO   = EdgeOrien eo }) =
  F.Cube $ array F.boundsF $ corners ++ edges ++ centers
  where setFacelets p o f = zip (concat f)
                              $ concatMap (\c -> rotate (o !! c) $ f !! c) p
        corners = setFacelets cp co cornerFacelets
        edges   = setFacelets ep eo edgeFacelets
        centers = [(x,x) | x <- [5,14..50]]

fromColorCube :: F.ColorCube -> Cube
fromColorCube (F.ColorCube cc) =
  Cube { cornerP = CornerPermu cp,
         cornerO = CornerOrien co,
         edgeP   = EdgePermu ep,
         edgeO   = EdgeOrien eo }
  where (cp, co) = unzip $ map (pAndO cornerColors 0 . map (cc !))
                                 cornerFacelets
        (ep, eo) = unzip $ map (pAndO edgeColors 0 . map (cc !)) edgeFacelets
        cornerColors = map (map F.color) cornerFacelets
        edgeColors = map (map F.color) edgeFacelets
        pAndO l o colors | o < 3     = case elemIndex colors l of
                                         Nothing -> pAndO l (o+1)
                                                            (rotate 1 colors)
                                         Just i  -> (i+1, o)
                         | otherwise = undefined

-- Elementary moves

move cp' co' ep' eo' = cube cp co ep eo
  where cp = CornerPermu $ map (lookupOrf cp' id) [0..numCorners-1]
        ep = EdgePermu   $ map (lookupOrf ep' id) [0..numEdges-1]
        co = CornerOrien $ map (lookupOrf co' (const 0)) [0..numCorners-1]
        eo = EdgeOrien   $ map (lookupOrf eo' (const 0)) [0..numEdges-1]
        lookupOrf m f i = case lookup i m of
                            Nothing -> f i
                            Just j  -> j

uCubie = move [( 0, 1), ( 1, 2), ( 2, 3), ( 3, 0)] []
              [( 0, 1), ( 1, 2), ( 2, 3), ( 3, 0)] []

dCubie = move [( 4, 7), ( 5, 4), ( 6, 5), ( 7, 6)] []
              [( 4, 7), ( 5, 4), ( 6, 5), ( 7, 6)] []
