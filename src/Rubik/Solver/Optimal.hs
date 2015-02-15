module Rubik.Solver.Optimal where

import Rubik.Cube
import Rubik.IDA
import Rubik.Misc
import Rubik.Solver

import Control.Applicative

import Data.Binary.Store
import Data.StrictTuple

optim dist = extract . search optimSearch . encodeCI' optimCI
  where
    optimSearch = searchWith move18Names optimCI transpose9 $ optimDist dist

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

optimDistTables
  = [ dist_CornerOrien_UDSlice,
      dist_CornerOrien_LRSlice,
      dist_CornerOrien_FBSlice,
      dist_EdgeOrien_UDSlice,
      dist_EdgeOrien_LRSlice,
      dist_EdgeOrien_FBSlice ]

-- ** Optimal solver
optimDist dist =
  zip (dist_CornerPermu : dist)
    [ One cp,
      Two rUDS (co, uds),
      Two rUDS (co, lrs),
      Two rUDS (co, fbs),
      Two rUDS (eo, uds),
      Two rUDS (eo, lrs),
      Two rUDS (eo, fbs) ]
  where
    rUDS = range coordUDSlice
    rEO = range coordEdgeOrien
    cp : co : eo : uds : lrs : fbs : udp : lrp : fbp : _ = [0 ..]

