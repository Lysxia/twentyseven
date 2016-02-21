{- |
   Cubie representation.

   A Rubik's cube is the cartesian product of a permutation of cubies
   and an action on their orientations.
-}

module Rubik.Cube.Cubie (
  -- * Complete cube
  CubeAction (..),
  FromCube (..),
  Cube (..),

  -- ** Solvability test
  solvable,

  -- * Corners
  numCorners,
  CornerPermu,
  CornerOrien,
  Corner (..),

  -- ** (De)construction
  cornerPermu,
  cornerOrien,
  fromCornerPermu,
  fromCornerOrien,

  -- * Edges
  numEdges,
  EdgePermu,
  EdgeOrien,
  Edge (..),

  -- ** (De)construction
  edgePermu,
  edgeOrien,
  fromEdgePermu,
  fromEdgeOrien,

  -- * Conversions
  stringOfCubeColors,
  toFacelet,
  colorFaceletsToCube,

  -- * UDSlice
  numUDSliceEdges,
  UDSlicePermu,
  UDSlice,
  UDSlicePermu2,
  UDEdgePermu2,
  FlipUDSlice,
  FlipUDSlicePermu,

  -- ** (De)construction
  uDSlicePermu,
  uDSlice,
  uDSlicePermu2,
  uDEdgePermu,
  edgePermu2,
  fromUDSlicePermu,
  fromUDSlice,
  fromUDSlicePermu2,
  fromUDEdgePermu2,

  -- ** Symmetry
  conjugateUDSlicePermu,
  conjugateFlipUDSlice,
  conjugateFlipUDSlicePermu,
  conjugateCornerOrien
  ) where

import Rubik.Cube.Cubie.Internal
