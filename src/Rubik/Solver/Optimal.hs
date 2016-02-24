module Rubik.Solver.Optimal where

import Rubik.Cube
import Rubik.IDA
import Rubik.Misc
import Rubik.Solver

import Control.Applicative

import Data.Maybe
import Data.StrictTuple

optim = do
  proj
    <-   proj18CornerOrien
    |:|. proj18EdgeOrien
    |:|. proj18UDSlice
    |:|. proj18LRSlice
    |:|. proj18FBSlice
    |:|. proj18UDSlicePermu
    |:|. proj18LRSlicePermu
    |.|. proj18FBSlicePermu
  dist <- optimDistTables
    <$> dist_CornerOrien_UDSlice
    <*> dist_CornerOrien_LRSlice
    <*> dist_CornerOrien_FBSlice
    <*> dist_EdgeOrien_UDSlice
    <*> dist_EdgeOrien_LRSlice
    <*> dist_EdgeOrien_FBSlice
  let optimSearch = mkSearch move18Names proj dist
      convert = (,) 6 . convertP proj
  return $ fromJust . search optimSearch . convert

optimDistTables d_co_uds d_co_lrs d_co_fbs d_eo_uds d_eo_lrs d_eo_fbs = maxDistance
  [ (\(Tuple8 co _ uds _ _ _ _ _) -> (co, uds)) >$< d_co_uds
  , (\(Tuple8 co _ _ lrs _ _ _ _) -> (co, lrs)) >$< d_co_lrs
  , (\(Tuple8 co _ _ _ fbs _ _ _) -> (co, fbs)) >$< d_co_fbs
  , (\(Tuple8 _ eo uds _ _ _ _ _) -> (eo, uds)) >$< d_eo_uds
  , (\(Tuple8 _ eo _ lrs _ _ _ _) -> (eo, lrs)) >$< d_eo_lrs
  , (\(Tuple8 _ eo _ _ fbs _ _ _) -> (eo, fbs)) >$< d_eo_fbs
  ]
