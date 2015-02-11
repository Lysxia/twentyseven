module Solver.Optimal where

import Coord
import IDA
import Misc
import Moves
import Solver
import StrictTuple

import Data.Functor

optim = extract . search optimSearch . encodeCI' optimCI
  where
    optimSearch = searchWith move18Names optimCI transpose9 optimDist

-- ** Optimal solver
optimCI = Tuple9
    move18CornerPermu
    move18CornerOrien
    move18EdgeOrien
    move18UDSlice
    move18LRSlice
    move18FBSlice
    move18UDSlicePermu
    move18LRSlicePermu
    move18FBSlicePermu

-- ** Optimal solver
optimDist =
  [
    (dist_CornerPermu, One cp),
    (dist_CornerOrien_UDSlice, Two rUDS (co, uds)),
    (dist_CornerOrien_LRSlice, Two rUDS (co, lrs)),
    (dist_CornerOrien_FBSlice, Two rUDS (co, fbs))--,
--    (dist_EdgeOrien_UDSlice, Two rUDS (eo, uds)),
--    (dist_EdgeOrien_LRSlice, Two rUDS (eo, lrs)),
--    (dist_EdgeOrien_FBSlice, Two rUDS (eo, fbs))
  ]
  where
    rUDS = range coordUDSlice
    rEO = range coordEdgeOrien
    cp : co : eo : uds : lrs : fbs : udp : lrp : fbp : _ = [0 ..]

optimTables
  = (listSeq <$> movesCI <$> optimCI) `seq`
    dist_CornerOrien_UDSlice `seq`
    dist_CornerOrien_LRSlice `seq`
    dist_CornerOrien_FBSlice `seq`
    dist_EdgeOrien_UDSlice `seq`
    dist_EdgeOrien_LRSlice `seq`
    dist_EdgeOrien_FBSlice `seq`
    ()

