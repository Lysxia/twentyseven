module Rubik.Solver.Optimal where

import Rubik.Cube
import Rubik.Solver
import Rubik.Tables.Moves
import Rubik.Tables.Distances
import Rubik.Tables.Internal

import qualified Data.Vector.Generic as G

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

{-# INLINE optiDist #-}
optiDist = maxDistance
  [ maxOrEqualPlusOne
      ( (\((,,,,,,) fudsp _ _ co _ _ _) -> (fudsp, co)) >$< fudsp_co
      , (\((,,,,,,) _ fudsp _ _ co _ _) -> (fudsp, co)) >$< fudsp_co
      , (\((,,,,,,) _ _ fudsp _ _ co _) -> (fudsp, co)) >$< fudsp_co
      )
  , (\((,,,,,,) _ _ _ co _ _ cp) -> (cp, co)) >$< cp_co
  ]

{-# INLINE maxOrEqualPlusOne #-}
maxOrEqualPlusOne (Distance f, Distance g, Distance h)
  = Distance $ \x -> let a = f x ; b = g x ; c = h x
    in if a == b && b == c && a /= 0 then a + 1
      else a `max` b `max` c

solve :: Cube -> Move
solve = solveWith move18Names moves optiProj optiDist
  where
    moves = (,,,,,,) m_fudsp m_fudsp m_fudsp m_co m_co m_co move18SymCornerPermu
    m_fudsp = move18SymFlipUDSlicePermu
    m_co = move18CornerOrien

{-# INLINE toIdx #-}
toIdx = uncurry $ indexWithSym invertedSym16CornerOrien (range ([] :: [CornerOrien]))

{-# INLINE fudsp_co #-}
fudsp_co = toIdx >$< Distance (fromIntegral . (dSym_CornerOrien_FlipUDSlicePermu G.!))
{-# INLINE cp_co #-}
cp_co = toIdx >$< Distance (dSym_CornerOrien_CornerPermu G.!)
