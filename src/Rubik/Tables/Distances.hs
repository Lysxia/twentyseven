module Rubik.Tables.Distances where

import Rubik.Cube
import Rubik.Solver
import Rubik.Tables.Internal
import Rubik.Tables.Moves
import qualified Data.Vector.Storable.Allocated as S
import qualified Data.Vector.HalfByte as HB

d_CornerOrien_UDSlice
  = distanceTable2 "dist_CornerOrien_UDSlice" move18CornerOrien move18UDSlice

d_EdgeOrien_UDSlice
  = distanceTable2 "dist_EdgeOrien_UDSlice" move18EdgeOrien move18UDSlice

d_UDEdgePermu2_UDSlicePermu2
  = distanceTable2 "dist_EdgePermu2" move10UDEdgePermu2 move10UDSlicePermu2

d_CornerPermu_UDSlicePermu2
  = distanceTable2 "dist_CornerPermu_UDSlicePermu2" move10CornerPermu move10UDSlicePermu2

dSym_CornerOrien_FlipUDSlicePermu
  = saved' "dist_SymFlipUDSlicePermu_CornerOrien" $
      distanceWithSym2'
        move18SymFlipUDSlicePermu move18CornerOrien
        invertedSym16CornerOrien
        symProjFlipUDSlicePermu
        rawProjection
        n1
        n2
  :: HB.Vector'
  where
    n1 = 1523864
    n2 = range ([] :: [CornerOrien])

dSym_CornerOrien_CornerPermu
  = saved' "dist_SymCornerPermu_CornerOrien" $
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
