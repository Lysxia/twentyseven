{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
module Rubik.Tables.Moves where

import Control.Monad
import Control.Monad.ST

import Rubik.Cube
import Rubik.Misc
import Rubik.Solver
import Rubik.Symmetry
import Rubik.Tables.Internal

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

move18CornerOrien
  = savedRawMoveTables "move18CornerOrien" move18 rawCornerOrien

move18CornerPermu
  = savedRawMoveTables "move18CornerPermu" move18 rawCornerPermu

move18EdgeOrien
  = savedRawMoveTables "move18EdgeOrien" move18 rawEdgeOrien

move18UDSlicePermu
  = savedRawMoveTables "move18UDSlicePermu" move18 rawUDSlicePermu

move18UDSlice = savedRawMoveTables "move18UDSlice" move18 rawUDSlice

move10CornerPermu = move18to10 move18CornerPermu

move10UDSlicePermu2
  = savedRawMoveTables "move10UDSlicePermu2" move10 rawUDSlicePermu2

move10UDEdgePermu2
  = savedRawMoveTables "move10UDEdgePermu2" move10 rawUDEdgePermu2

--move18SymFlipUDSlicePermu
--  = (savedSymMoveTables
--      "move10symFlipUDSlicePermu"
--      move18
--      rawFlipUDSlicePermu
--      actionFlipUDSlicePermu
--      symReprFlipUDSlicePermu
--      conjugateFlipUDSlicePermu)

sym16CornerOrien
  = savedRawSymTables "sym16CornerOrien" conjugateCornerOrien sym16 rawCornerOrien

symProjCornerPermu
  = symProjection rawCornerPermu toSymCoord
  where
    toSymCoord = rawToSymCornerPermu . encode rawCornerPermu

move18SymCornerPermu :: MoveTag Move18 [SymMove UDFix CornerPermu]
move18SymCornerPermu
  = saved "move18SymCornerPermu" . MoveTag $ fmap
      (\moveCP ->
        SymMove . U.map (f moveCP) $ unSymClassTable classCornerPermu)
      (unMoveTag move18CornerPermu)
  where
    f (RawMove moveCP) = (\(SymClass c, SymCode i) -> flatIndex 16 c i) . rawToSymCornerPermu . RawCoord . (moveCP U.!)

symProjFlipUDSlicePermu
  = symProjection rawFlipUDSlicePermu toSymCoord
  where
    toSymCoord (udsp, eo) =
      rawToSymFlipUDSlicePermu (encode rawUDSlicePermu udsp) (encode rawEdgeOrien eo)

rawToSymCornerPermu (RawCoord x) = (SymClass c, SymCode i)
  where
    (r, i) = (unSymReprTable reprCornerPermu U.! x) `divMod` 16
    c = fromJust . iFind r $ unSymClassTable classCornerPermu

classCornerPermu
  = saved "classCornerPermu" $ symClassTable 16 reprCornerPermu

reprCornerPermu
  = saved "reprCornerPermu" $ SymReprTable reprCornerPermu'

reprCornerPermu'
  = symReprTable' (range rawCornerPermu) 16 $
      \(decode rawCornerPermu . RawCoord -> cp) ->
        [ unRawCoord . encode rawCornerPermu $ conj s cp
        | s <- sym16' ]
  where
    conj (fromCube -> s) cp = inverse s <> cp <> s

move18SymFlipUDSlicePermu :: MoveTag Move18 [SymMove UDFix FlipUDSlicePermu]
move18SymFlipUDSlicePermu
  = saved "move18SymFlipUDSlicePermu" .  MoveTag $ zipWith
      (\moveUDSP moveEO ->
        SymMove $ U.map (f moveUDSP moveEO) (unSymClassTable classFlipUDSlicePermu))
      (unMoveTag move18UDSlicePermu) (unMoveTag move18EdgeOrien)
  where
    nEO = range rawEdgeOrien
    f (RawMove moveUDSP) (RawMove moveEO) x =
      let (i, j) = x `divMod` nEO
      in unSymReprTable reprFlipUDSlicePermu U.!
        flatIndex nEO (moveUDSP U.! i) (moveEO U.! j)

rawToSymFlipUDSlicePermu
  :: RawCoord UDSlicePermu -> RawCoord EdgeOrien -> SymCoord UDFix FlipUDSlicePermu
rawToSymFlipUDSlicePermu (RawCoord x) (RawCoord y) = (SymClass c, SymCode i)
  where
    (r, i) = (unSymReprTable reprFlipUDSlicePermu U.! flatIndex nEO x y) `divMod` 16
    c = fromJust . iFind r $ unSymClassTable classFlipUDSlicePermu
    nEO = range rawEdgeOrien

classFlipUDSlicePermu :: SymClassTable UDFix FlipUDSlicePermu
classFlipUDSlicePermu
  = saved "classFlipUDSlicePermu" $ symClassTable 16 reprFlipUDSlicePermu

reprFlipUDSlicePermu :: SymReprTable UDFix FlipUDSlicePermu
reprFlipUDSlicePermu
  = saved "reprFlipUDSlicePermu" $ SymReprTable reprFlipUDSlicePermu'

reprFlipUDSlicePermu' :: U.Vector Int
reprFlipUDSlicePermu'
  = symReprTable' (range rawUDSlicePermu * range rawEdgeOrien) 16 $
      \((`divMod` range rawEdgeOrien) -> (i, j)) ->
        let conjUDSP = conjUDSlicePermu V.! i
            udspComp = udspComponentOfConjEdgeOrien V.! i
            eoComp = eoComponentOfConjEdgeOrien V.! j
        in zipWith4 toCoord conjUDSP udspComp eoComp cubeComp
  where
    toCoord (RawCoord coordUDSP) udspComp eoComp cubeComp =
      let RawCoord coordEO = encode rawEdgeOrien . unsafeEdgeOrien $
            U.zipWith3 (\a b c -> (a + b + c) `mod` 2) udspComp eoComp cubeComp
      in flatIndex (range rawEdgeOrien) coordUDSP coordEO
    cubeComp = cubeComponentOfConjEdgeOrien

-- x :: UDSlicePermu -> [ s^(-1) <> x <> s | s <- symUDFix ]
conjUDSlicePermu :: V.Vector [RawCoord UDSlicePermu]
conjUDSlicePermu = V.generate range $ \i ->
  [ encode . conjugateUDSlicePermu c . decode $ RawCoord i | c <- sym16' ]
  where RawEncoding{..} = rawUDSlicePermu

type EOComponent = U.Vector Int

udspComponentOfConjEdgeOrien :: V.Vector [EOComponent]
udspComponentOfConjEdgeOrien
  = V.generate range $ \i ->
      let udsp = fromUDSlicePermu . decode $ RawCoord i
      in map (orien udsp) sym16'
  where
    RawEncoding{..} = rawUDSlicePermu
    orien udsp c =
      let (fromEdgeOrien -> eo_c, fromEdgePermu -> ep_c) = fromCube c
          altO = eo_c U.! 0
          udsO = eo_c U.! 8
      in U.map (\p -> bool altO udsO (p `U.elem` udsp)) ep_c

eoComponentOfConjEdgeOrien :: V.Vector [EOComponent]
eoComponentOfConjEdgeOrien
  = V.generate range $ \j ->
      let eo = fromEdgeOrien . decode $ RawCoord j
      in map (orien eo) sym16'
  where
    RawEncoding{..} = rawEdgeOrien
    orien eo = U.map (eo U.!) . fromEdgePermu . fromCube

cubeComponentOfConjEdgeOrien :: [EOComponent]
cubeComponentOfConjEdgeOrien = map (fromEdgeOrien . fromCube) sym16'
