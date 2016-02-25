module Rubik.Solver.Optimal where

import Rubik.Cube
import Rubik.IDA
import Rubik.Misc
import Rubik.Solver
import Rubik.Symmetry
import Rubik.Tables.Moves
import Rubik.Tables.Distances
import Rubik.Tables.Internal

import Data.Function ( on )
import Data.Maybe
import Data.Monoid
import Data.StrictTuple
import qualified Data.Vector.Primitive as P

{-# INLINE optiProj #-}
optiProj
  = fudsp |*| sfudsp |*| s sfudsp |*| co |*| sco |*| s sco |.| cp
  where
    fudsp = symProjFlipUDSlicePermu
    sfudsp = s fudsp
    co = rawProjection :: Projection' Move18 CornerOrien
    sco = s co
    cp = symProjCornerPermu
    s x = symmetricProj symmetry_urf3 x

optiConvert = convertP optiProj

{-# INLINE optiDist #-}
optiDist = maxDistance
  [ maxOrEqualPlusOne
      ( (\(Tuple7 fudsp _ _ co _ _ _) -> (fudsp, co)) >$< fudsp_co
      , (\(Tuple7 _ fudsp _ _ co _ _) -> (fudsp, co)) >$< fudsp_co
      , (\(Tuple7 _ _ fudsp _ _ co _) -> (fudsp, co)) >$< fudsp_co
      )
  , (\(Tuple7 _ _ _ co _ _ cp) -> (cp, co)) >$< cp_co
  ]

{-# INLINE maxOrEqualPlusOne #-}
maxOrEqualPlusOne (Distance f, Distance g, Distance h)
  = Distance $ \x -> let a = f x ; b = g x ; c = h x
    in if a == b && b == c && a /= 0 then a + 1
      else a `max` b `max` c

solver :: Cube -> Move
solver =
    let moves = Tuple7 m_fudsp m_fudsp m_fudsp m_co m_co m_co move18SymCornerPermu
        m_fudsp = move18SymFlipUDSlicePermu
        m_co = move18CornerOrien
        optiSearch = mkSearch move18Names moves optiProj optiDist
    in fromJust . search optiSearch . tag . optiConvert

{-# INLINE toIdx #-}
toIdx = uncurry $ indexWithSym invertedSym16CornerOrien (range ([] :: [CornerOrien]))

{-# INLINE fudsp_co #-}
fudsp_co = toIdx >$< Distance (dSym_CornerOrien_FlipUDSlicePermu P.!)
{-# INLINE cp_co #-}
cp_co = toIdx >$< Distance (dSym_CornerOrien_CornerPermu P.!)
