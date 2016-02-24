{-# LANGUAGE RecordWildCards, ScopedTypeVariables, ViewPatterns #-}
module Rubik.Tables.Moves where

import Rubik.Cube
import Rubik.Cube.Cubie.Internal
import Rubik.Misc
import Rubik.Solver
import Rubik.Symmetry
import Rubik.Tables.Internal

import Control.Monad
import Control.Monad.ST
import Control.Newtype

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Pinned as P

type Moves m a = MoveTag m [RawMove a]

move18CornerOrien = savedRawMoveTables "move18CornerOrien" move18
  :: Moves Move18 CornerOrien

move18CornerPermu = savedRawMoveTables "move18CornerPermu" move18
  :: Moves Move18 CornerPermu

move18EdgeOrien = savedRawMoveTables "move18EdgeOrien" move18
  :: Moves Move18 EdgeOrien

move18UDSlicePermu = savedRawMoveTables "move18UDSlicePermu" move18
  :: Moves Move18 UDSlicePermu

move18UDSlice = savedRawMoveTables "move18UDSlice" move18
  :: Moves Move18 UDSlice

move10CornerPermu = move18to10 move18CornerPermu
  :: Moves Move10 CornerPermu

move10UDSlicePermu2 = savedRawMoveTables "move10UDSlicePermu2" move10
  :: Moves Move10 UDSlicePermu2

move10UDEdgePermu2 = savedRawMoveTables "move10UDEdgePermu2" move10
  :: Moves Move10 UDEdgePermu2

--move18SymFlipUDSlicePermu
--  = (savedSymMoveTables
--      "move10symFlipUDSlicePermu"
--      move18
--      rawFlipUDSlicePermu
--      actionFlipUDSlicePermu
--      symReprFlipUDSlicePermu
--      conjugateFlipUDSlicePermu)

sym16CornerOrien
  = savedRawSymTables "sym16CornerOrien" conjugateCornerOrien sym16
  :: Moves UDFix CornerOrien

symProjCornerPermu = symProjection (rawToSymCornerPermu . encode)
  :: SymProjection Move18 UDFix CornerPermu

move18SymCornerPermu :: MoveTag Move18 [SymMove UDFix CornerPermu]
move18SymCornerPermu
  = savedVectorList_ "move18SymCornerPermu" . MoveTag $ fmap
      (\moveCP ->
        SymMove . P.mapPinned (f moveCP) $ unSymClassTable classCornerPermu)
      (unMoveTag move18CornerPermu)
  where
    f (RawMove moveCP) = (\(SymClass c, SymCode i) -> flatIndex 16 c i) . rawToSymCornerPermu . RawCoord . (moveCP P.!)

symProjFlipUDSlicePermu
  = symProjection toSymCoord
  where
    toSymCoord = rawToSymFlipUDSlicePermu . encode

rawToSymCornerPermu (RawCoord x) = (SymClass c, SymCode i)
  where
    (r, i) = (unSymReprTable reprCornerPermu P.! x) `divMod` 16
    c = fromJust . iFind r $ unSymClassTable classCornerPermu

classCornerPermu
  = savedVector_ "classCornerPermu" $ symClassTable 16 reprCornerPermu

reprCornerPermu
  = savedVector_ "reprCornerPermu" $ SymReprTable reprCornerPermu'

reprCornerPermu'
  = symReprTable' (range ([] :: [CornerPermu])) 16 $
      \cp -> [ under RawCoord (encode . conj s . decode) cp | s <- sym16' ]
  where
    conj (fromCube -> s) (cp :: CornerPermu) = inverse s <> cp <> s

move18SymFlipUDSlicePermu :: MoveTag Move18 [SymMove UDFix FlipUDSlicePermu]
move18SymFlipUDSlicePermu
  = savedVectorList' 18 1523864 "move18SymFlipUDSlicePermu" . MoveTag $ zipWith
      (\moveUDSP moveEO ->
        SymMove $ P.mapPinned (f moveUDSP moveEO) (unSymClassTable classFlipUDSlicePermu))
      (unMoveTag move18UDSlicePermu) (unMoveTag move18EdgeOrien)
  where
    nEO = range ([] :: [EdgeOrien])
    f (RawMove moveUDSP) (RawMove moveEO) x =
      let (i, j) = x `divMod` nEO
          z = flatIndex nEO (moveUDSP P.! i) (moveEO P.! j)
          (SymClass c, SymCode s) = rawToSymFlipUDSlicePermu (RawCoord z)
      in flatIndex 16 c s

rawToSymFlipUDSlicePermu
  :: RawCoord FlipUDSlicePermu -> SymCoord UDFix FlipUDSlicePermu
rawToSymFlipUDSlicePermu (RawCoord z) = (SymClass c, SymCode i)
  where
    (r, i) = (unSymReprTable reprFlipUDSlicePermu P.! z) `divMod` 16
    c = fromJust . iFind r $ unSymClassTable classFlipUDSlicePermu

rawToSymFlipUDSlicePermu'
  :: RawCoord UDSlicePermu -> RawCoord EdgeOrien
  -> SymCoord UDFix FlipUDSlicePermu
rawToSymFlipUDSlicePermu' (RawCoord x) (RawCoord y)
  = rawToSymFlipUDSlicePermu (RawCoord (flatIndex nEO x y))
  where
    nEO = range ([] :: [EdgeOrien])

classFlipUDSlicePermu :: SymClassTable UDFix FlipUDSlicePermu
classFlipUDSlicePermu
  = savedVector' 1523864 "classFlipUDSlicePermu" $ symClassTable 16 reprFlipUDSlicePermu

reprFlipUDSlicePermu :: SymReprTable UDFix FlipUDSlicePermu
reprFlipUDSlicePermu
  = savedVector' 24330240 "reprFlipUDSlicePermu" $ SymReprTable reprFlipUDSlicePermu'

reprFlipUDSlicePermu' :: P.Vector Int
reprFlipUDSlicePermu'
  = symReprTable' (nUDSP * nEO) 16 $
      \((`divMod` nEO) -> (i, j)) ->
        let fudsps = conjugateFlipUDSlicePermu_ (RawCoord i) (RawCoord j)
        in fmap toCoord fudsps
  where
    nUDSP = range ([] :: [UDSlicePermu])
    nEO = range ([] :: [EdgeOrien]) 
    toCoord (RawCoord coordUDSP, encode -> RawCoord coordEO) =
      flatIndex nEO coordUDSP coordEO

conjugateFlipUDSlicePermu'
  :: SymCode UDFix -> FlipUDSlicePermu -> FlipUDSlicePermu
conjugateFlipUDSlicePermu' (SymCode c) (udsp, eo)
  = first decode (conjugateFlipUDSlicePermu_ i j !! c)
  where
    i = encode udsp
    j = encode eo

conjugateFlipUDSlicePermu_
  :: RawCoord UDSlicePermu -> RawCoord EdgeOrien
  -> [(RawCoord UDSlicePermu, EdgeOrien)]
conjugateFlipUDSlicePermu_ (RawCoord i) (RawCoord j)
  = zipWith4 f conjUDSP udspComp eoComp cubeComp
  where
    conjUDSP = conjUDSlicePermu V.! i
    udspComp = udspComponentOfConjEdgeOrien V.! i
    eoComp = eoComponentOfConjEdgeOrien V.! j
    cubeComp = cubeComponentOfConjEdgeOrien
    f conjUDSP udspComp eoComp cubeComp
      = ( conjUDSP
        , unsafeEdgeOrien $
            U.zipWith3 (\a b c -> (a+b+c) `mod` 2) udspComp eoComp cubeComp
        )

conjugateUDSlicePermu'
  :: SymCode UDFix -> UDSlicePermu -> UDSlicePermu
conjugateUDSlicePermu' (SymCode c) udsp
  = decode (conjUDSlicePermu V.! i !! c)
  where RawCoord i = encode udsp

-- x :: UDSlicePermu -> [ s^(-1) <> x <> s | s <- symUDFix ]
conjUDSlicePermu :: V.Vector [RawCoord UDSlicePermu]
conjUDSlicePermu = V.generate (range ([] :: [UDSlicePermu])) $ \i ->
  [ encode . conjugateUDSlicePermu c . decode $ RawCoord i | c <- sym16' ]

type EOComponent = U.Vector Int

udspComponentOfConjEdgeOrien :: V.Vector [EOComponent]
udspComponentOfConjEdgeOrien
  = V.generate (range ([] :: [UDSlicePermu])) $ \i ->
      let udsp = fromUDSlicePermu . decode $ RawCoord i
      in map (orien udsp) sym16'
  where
    orien udsp c =
      let (fromEdgeOrien -> eo_c, fromEdgePermu -> ep_c) = fromCube c
          altO = eo_c U.! 0
          udsO = eo_c U.! 8
      in U.map (\p -> bool altO udsO (p `U.elem` udsp)) ep_c

eoComponentOfConjEdgeOrien :: V.Vector [EOComponent]
eoComponentOfConjEdgeOrien
  = V.generate (range ([] :: [EdgeOrien])) $ \j ->
      let eo = fromEdgeOrien . decode $ RawCoord j
      in map (orien eo) sym16'
  where
    orien eo = U.map (eo U.!) . fromEdgePermu . fromCube

cubeComponentOfConjEdgeOrien :: [EOComponent]
cubeComponentOfConjEdgeOrien = map (fromEdgeOrien . fromCube) sym16'
