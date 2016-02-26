{-# LANGUAGE TemplateHaskell #-}
module Rubik.Tables.Distances where

import Rubik.Solver
import Rubik.Tables.Internal
import Rubik.Tables.Moves
import Rubik.Cube
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Allocated as S

d_CornerOrien_UDSlice
  = distanceTable2 "dist_CornerOrien_UDSlice" move18CornerOrien move18UDSlice

d_EdgeOrien_UDSlice
  = distanceTable2 "dist_EdgeOrien_UDSlice" move18EdgeOrien move18UDSlice

d_UDEdgePermu2_UDSlicePermu2
  = distanceTable2 "dist_EdgePermu2" move10UDEdgePermu2 move10UDSlicePermu2

d_CornerPermu_UDSlicePermu2
  = distanceTable2 "dist_CornerPermu_UDSlicePermu2" move10CornerPermu move10UDSlicePermu2

dSym_CornerOrien_FlipUDSlicePermu
  = savedHBVector (n1 * n2) "dSym_CornerOrien_FlipUDSlicePermu" $
      distanceWithSym2'
        move18SymFlipUDSlicePermu move18CornerOrien
        (MoveTag $ V.fromList
          [ unMoveTag sym16CornerOrien !! j
          | i <- [0 .. 15], let SymCode j = invertSym (SymCode i) ])
        symProjFlipUDSlicePermu
        rawProjection
        n1
        n2
  where
    n1 = 1523864
    n2 = range ([] :: [CornerOrien])

dSym_CornerOrien_CornerPermu
  = savedVector (n1 * n2) "dSym_CornerOrien_CornerPermu" $
      distanceWithSym2'
        move18SymCornerPermu move18CornerOrien
        invertedSym16CornerOrien
        symProjCornerPermu
        rawProjection
        n1
        n2
  :: S.Vector DInt
  where
    n1 = 2768
    n2 = range ([] :: [CornerOrien])
