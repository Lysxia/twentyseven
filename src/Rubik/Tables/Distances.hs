{-# LANGUAGE TemplateHaskell #-}
module Rubik.Tables.Distances where

import Rubik.Solver
import Rubik.Symmetry
import Rubik.Tables.Internal
import Rubik.Tables.Moves
import Rubik.Cube
import Rubik.Misc
import Data.Coerce
import Data.Int ( Int8 )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Primitive as P

d_CornerOrien_UDSlice
  = (distanceTable2 "dist_CornerOrien_UDSlice" move18CornerOrien move18UDSlice projCornerOrien projUDSlice rawCornerOrien rawUDSlice)

d_EdgeOrien_UDSlice
  = (distanceTable2 "dist_EdgeOrien_UDSlice" move18EdgeOrien move18UDSlice projEdgeOrien projUDSlice rawEdgeOrien rawUDSlice)

d_UDEdgePermu2_UDSlicePermu2
  = (distanceTable2 "dist_EdgePermu2" move10UDEdgePermu2 move10UDSlicePermu2 projUDEdgePermu2 projUDSlicePermu2 rawUDEdgePermu2 rawUDSlicePermu2)

d_CornerPermu_UDSlicePermu2
  = (distanceTable2 "dist_CornerPermu_UDSlicePermu2" move10CornerPermu move10UDSlicePermu2 projCornerPermu projUDSlicePermu2 rawCornerPermu rawUDSlicePermu2)

dSym_CornerOrien_FlipUDSlicePermu
  = savedVector (n1 * n2) "dSym_CornerOrien_FlipUDSlicePermu" $
      distanceWithSym2'
        move18SymFlipUDSlicePermu move18CornerOrien
        (MoveTag $ V.fromList
          [ unMoveTag sym16CornerOrien !! j
          | i <- [0 .. 15], let SymCode j = invertSym (SymCode i) ])
        symProjFlipUDSlicePermu
        projCornerOrien
        n1
        n2
  where
    n1 = P.length (unSymClassTable classFlipUDSlicePermu)
    n2 = range rawCornerOrien

dSym_CornerOrien_CornerPermu
  = savedVector (n1 * n2) "dSym_CornerOrien_CornerPermu" $
      distanceWithSym2'
        move18SymCornerPermu move18CornerOrien
        (MoveTag $ V.fromList
          [ unMoveTag sym16CornerOrien !! j
          | i <- [0 .. 15], let SymCode j = invertSym (SymCode i) ])
        symProjCornerPermu
        projCornerOrien
        n1
        n2
  where
    n1 = P.length (unSymClassTable classCornerPermu)
    n2 = range rawCornerOrien
