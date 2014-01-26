module Cubie where

import Data.Array.Unboxed
import Data.List
import qualified Facelet as F
import Misc

-- Cubie representation, replaced-by representation

newtype CornerPermu = CornerPermu (UArray Int Int)
newtype CornerOrien = CornerOrien (UArray Int Int)
newtype CornerCubie = CornerCubie (CornerPermu, CornerOrien)

newtype EdgePermu = EdgePermu (UArray Int Int)
newtype EdgeOrien = EdgeOrien (UArray Int Int)
newtype EdgeCubie = EdgeCubie (EdgePermu, EdgeOrien)

data Cube =
  Cube { cornerP :: CornerPermu,
         cornerO :: CornerOrien,
         edgeP   :: EdgePermu,
         edgeO   :: EdgeOrien
       }

cube cp co ep eo = Cube { cornerP = cp, cornerO = co, edgeP = ep, edgeO = eo }
cubie cp co ep eo = (CornerCubie (cp, co), EdgeCubie (ep, eo))

boundsC :: (Int, Int)
boundsC = (1, 8)

boundsE :: (Int, Int)
boundsE = (1, 12)

idCornerP = CornerPermu $ idArray boundsC
idCornerO = CornerOrien $ listArray boundsC $ replicate 8 0
idEdgeP = EdgePermu $ idArray boundsE
idEdgeO = EdgeOrien $ listArray boundsE $ replicate 12 0

instance Group CornerPermu where
  id = idCornerP
  (CornerPermu a) `compose` (CornerPermu b) = CornerPermu $ composeArray a b

instance Group CornerCubie where
  id = CornerCubie (idCornerP, idCornerO)
  compose (CornerCubie (a@(CornerPermu ap), CornerOrien ao))
          (CornerCubie (b,                  CornerOrien bo)) =
    CornerCubie (a `compose` b, CornerOrien o)
    where o = listArray boundsC
                        [((ao ! i) + (bo ! (ap ! i))) `div` 3 | i <- range boundsC]

instance Group EdgePermu where
  id = idEdgeP
  (EdgePermu a) `compose` (EdgePermu b) = EdgePermu $ composeArray a b

instance Group EdgeCubie where
  id = EdgeCubie (idEdgeP, idEdgeO)
  compose (EdgeCubie (a@(EdgePermu ap), EdgeOrien ao))
          (EdgeCubie (b,                EdgeOrien bo)) =
    EdgeCubie (a `compose` b, EdgeOrien o)
    where o = listArray boundsE
                        [((ao ! i) + (bo ! (ap ! i))) `div` 2 | i <- range boundsE]

instance Group Cube where
  id = cube idCornerP idCornerO idEdgeP idEdgeO
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
                              $ concatMap (\c -> rotate (o ! c)
                                                      $ f !! (c - 1))
                                        $ elems p
        corners = setFacelets cp co cornerFacelets
        edges = setFacelets ep eo edgeFacelets
        centers = map (\x -> (x,x)) [5,14..50]

fromColorCube :: F.ColorCube -> Cube
fromColorCube (F.ColorCube cc) =
  Cube { cornerP = cp, cornerO = co, edgeP = ep, edgeO = eo }
  where cp = CornerPermu $ listArray boundsC cp'
        co = CornerOrien $ listArray boundsC co'
        ep = EdgePermu $ listArray boundsE ep'
        eo = EdgeOrien $ listArray boundsE eo'
        (cp', co') = unzip $ map (pAndO cornerColors 0 . map (cc !))
                                 cornerFacelets
        (ep', eo') = unzip $ map (pAndO edgeColors 0 . map (cc !)) edgeFacelets
        cornerColors = map (map F.color) cornerFacelets
        edgeColors = map (map F.color) edgeFacelets
        pAndO l o colors | o < 3     = case elemIndex colors l of
                                         Nothing -> pAndO l (o+1)
                                                            (rotate 1 colors)
                                         Just i  -> (i+1, o)
                         | otherwise = undefined

-- Elementary moves

move cp' co' ep' eo' = cube cp co ep eo
  where cp = move' CornerPermu boundsC idC cp'
        co = move'' CornerOrien boundsC co'
        ep = move' EdgePermu boundsE idE ep'
        eo = move'' EdgeOrien boundsE eo'
        idC = idArray boundsC
        idE = idArray boundsE
        move' c bounds id m = c $ ixmap bounds (f m) id
        f m i = case lookup i m of
                Nothing -> i
                Just j  -> j
        move'' c bounds m = c $ listArray bounds (map (g m) (range bounds))
        g m i = if elem i m
                  then 1
                  else 0

uCubie = move [( 1, 2), ( 2, 3), ( 3, 4), ( 4, 1)] []
              [( 1, 2), ( 2, 3), ( 3, 4), ( 4, 1)] []

dCubie = move [( 5, 8), ( 6, 5), ( 7, 6), ( 8, 7)] []
              [( 5, 8), ( 6, 5), ( 7, 6), ( 8, 7)] []
