{-# LANGUAGE RecordWildCards, ScopedTypeVariables, ViewPatterns #-}
module Rubik.Tables.Moves where

import Rubik.Cube
import Rubik.Misc
import Rubik.Solver
import Rubik.Symmetry
import Rubik.Tables.Internal

import Data.Bifunctor
import Data.Bits
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable.Allocated as S

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
  :: Symmetries UDFix CornerOrien

invertedSym16CornerOrien
  = MoveTag $ V.fromList
          [ unMoveTag sym16CornerOrien V.! j
          | i <- [0 .. 15], let SymCode j = invertSym (SymCode i) ]
  :: Symmetries UDFix CornerOrien

sym16CornerPermu
  = savedRawSymTables "sym16CornerPermu" (conjugate . fromCube) sym16
  :: Symmetries UDFix CornerPermu

invertedSym16CornerPermu
  = MoveTag $ V.fromList
      [ unMoveTag sym16CornerPermu V.! j
      | i <- [0 .. 15], let SymCode j = invertSym (SymCode i) ]
  :: Symmetries UDFix CornerPermu

{-# INLINE symProjCornerPermu #-}
symProjCornerPermu
  = symProjection (rawToSymCornerPermu . encode)
  :: SymProjection Move18 UDFix CornerPermu

move18SymCornerPermu :: MoveTag Move18 [SymMove UDFix CornerPermu]
move18SymCornerPermu
  = saved' "move18SymCornerPermu" . MoveTag $ fmap
      (\moveCP ->
        SymMove . S.map (f moveCP) $ unSymClassTable classCornerPermu)
      (unMoveTag move18CornerPermu)
  where
    f (RawMove moveCP) = (\(SymClass c, SymCode i) -> flatIndex 16 c i) . rawToSymCornerPermu . RawCoord . (moveCP S.!)

{-# INLINE symProjFlipUDSlicePermu #-}
symProjFlipUDSlicePermu
  = symProjection
      (rawToSymFlipUDSlicePermu . encode)
  :: SymProjection Move18 UDFix FlipUDSlicePermu

rawToSymCornerPermu (RawCoord x) = (SymClass c, SymCode i)
  where
    (r, i) = (unSymReprTable reprCornerPermu S.! x) `divMod` 16
    c = fromJust . iFind r $ unSymClassTable classCornerPermu

{-# INLINE symToRawCornerPermu #-}
symToRawCornerPermu = symToRaw classCornerPermu (sym sym16CornerPermu)

classCornerPermu :: SymClassTable UDFix CornerPermu
classCornerPermu
  = saved' "classCornerPermu" $ symClassTable 16 reprCornerPermu

reprCornerPermu :: SymReprTable UDFix CornerPermu
reprCornerPermu
  = saved' "reprCornerPermu" $ symReprTable 16 $
      \cp -> [ (encode . conj s . decode) cp | s <- sym16' ]
  where
    conj (fromCube -> s) (cp :: CornerPermu) = inverse s <> cp <> s

move18SymFlipUDSlicePermu :: MoveTag Move18 [SymMove UDFix FlipUDSlicePermu]
move18SymFlipUDSlicePermu
  = saved' "move18SymFlipUDSlicePermu" . MoveTag $ zipWith
      (\moveUDSP moveEO ->
        SymMove $ S.map (f moveUDSP moveEO) (unSymClassTable classFlipUDSlicePermu))
      (unMoveTag move18UDSlicePermu) (unMoveTag move18EdgeOrien)
  where
    nEO = range ([] :: [EdgeOrien])
    f (RawMove moveUDSP) (RawMove moveEO) x =
      let (i, j) = x `divMod` nEO
          z = flatIndex nEO (moveUDSP S.! i) (moveEO S.! j)
          (SymClass c, SymCode s) = rawToSymFlipUDSlicePermu (RawCoord z)
      in flatIndex 16 c s

rawToSymFlipUDSlicePermu
  :: RawCoord FlipUDSlicePermu -> SymCoord UDFix FlipUDSlicePermu
rawToSymFlipUDSlicePermu (RawCoord z) = (SymClass c, SymCode i)
  where
    (r, i) = (unSymReprTable reprFlipUDSlicePermu S.! z) `divMod` 16
    c = fromJust . iFind r $ unSymClassTable classFlipUDSlicePermu

rawToSymFlipUDSlicePermu'
  :: RawCoord UDSlicePermu -> RawCoord EdgeOrien
  -> SymCoord UDFix FlipUDSlicePermu
rawToSymFlipUDSlicePermu'
  = rawToSymFlipUDSlicePermu .: flatCoord
  where (.:) = (.) (.) (.)

{-# INLINE symToRawFlipUDSlicePermu #-}
symToRawFlipUDSlicePermu = symToRaw classFlipUDSlicePermu $ \x (SymCode i) ->
    ( uncurry flatCoord
    . (V.! i)
    . uncurry conjugateFlipUDSlicePermu_
    . splitCoord
    ) x

classFlipUDSlicePermu :: SymClassTable UDFix FlipUDSlicePermu
classFlipUDSlicePermu
  = saved' "classFlipUDSlicePermu" $ symClassTable 16 reprFlipUDSlicePermu

reprFlipUDSlicePermu :: SymReprTable UDFix FlipUDSlicePermu
reprFlipUDSlicePermu
  = saved' "reprFlipUDSlicePermu" . symReprTable 16 $
      fmap (uncurry flatCoord) . uncurry conjugateFlipUDSlicePermu_ . splitCoord

conjugateFlipUDSlicePermu'
  :: SymCode UDFix -> FlipUDSlicePermu -> FlipUDSlicePermu
conjugateFlipUDSlicePermu' (SymCode c) (udsp, eo)
  = bimap decode decode (conjugateFlipUDSlicePermu_ i j V.! c)
  where
    i = encode udsp
    j = encode eo

{-# INLINE conjugateFlipUDSlicePermu_ #-}
conjugateFlipUDSlicePermu_
  :: RawCoord UDSlicePermu -> RawCoord EdgeOrien
  -> V.Vector (RawCoord UDSlicePermu, RawCoord EdgeOrien)
conjugateFlipUDSlicePermu_ (RawCoord i) (RawCoord j)
  = V.zipWith4 f conjUDSP udspComp eoComp cubeComp
  where
    conjUDSP = conjUDSlicePermu V.! i
    udspComp = udspComponentOfConjEdgeOrien V.! i
    eoComp = eoComponentOfConjEdgeOrien V.! j
    cubeComp = cubeComponentOfConjEdgeOrien
    f conjUDSP udspComp eoComp cubeComp
      = (conjUDSP, RawCoord (udspComp `xor` eoComp `xor` cubeComp))

conjugateUDSlicePermu'
  :: SymCode UDFix -> UDSlicePermu -> UDSlicePermu
conjugateUDSlicePermu' (SymCode c) udsp
  = decode (conjUDSlicePermu V.! i V.! c)
  where RawCoord i = encode udsp

-- x :: UDSlicePermu -> [ s^(-1) <> x <> s | s <- symUDFix ]
conjUDSlicePermu :: V.Vector (V.Vector (RawCoord UDSlicePermu))
conjUDSlicePermu = V.generate (range ([] :: [UDSlicePermu])) $ \i ->
  V.fromList [ encode . conjugateUDSlicePermu c . decode $ RawCoord i | c <- sym16' ]

-- | 11 bits describing edge orientations, as obtained by @encodeEdgeOrien'@
type EOComponent = Int
type EOComponents = V.Vector Int

udspComponentOfConjEdgeOrien :: V.Vector EOComponents
udspComponentOfConjEdgeOrien
  = V.generate (range ([] :: [UDSlicePermu])) $ \i ->
      let udsp = fromUDSlicePermu . decode $ RawCoord i
      in V.fromList $ map (encodeEdgeOrien' . orien udsp) sym16'
  where
    orien udsp c =
      let (fromEdgeOrien -> eo_c, fromEdgePermu -> ep_c) = fromCube c
          altO = eo_c U.! 0
          udsO = eo_c U.! 8
      in U.map (\p -> bool altO udsO (p `U.elem` udsp)) ep_c

eoComponentOfConjEdgeOrien :: V.Vector EOComponents
eoComponentOfConjEdgeOrien
  = V.generate (range ([] :: [EdgeOrien])) $ \j ->
      let eo = fromEdgeOrien . decode $ RawCoord j
      in V.fromList $ map (encodeEdgeOrien' . orien eo) sym16'
  where
    orien eo = U.backpermute eo . fromEdgePermu . fromCube

cubeComponentOfConjEdgeOrien :: EOComponents
cubeComponentOfConjEdgeOrien
  = V.fromList $ map (encodeEdgeOrien' . fromEdgeOrien . fromCube) sym16'
