{- | Move and cube definitions
 -}

module Rubik.Cube.Moves (
  MoveTag (..),
  Move18, Move10,

  -- * Generating moves
  u,r,f,d,l,b,
  move6,

  -- * 18 elementary moves
  move18Names,
  move18,

  -- * Other subgroups
  move10Names,
  move10,
  move6',

  -- * Symmetries
  surf3, sf2, su4, slr2,

  Symmetry (..),
  Symmetric,
  UDFix,
  rawMoveSym,
  rawCast,

  symmetry_urf3,
  symmetry_urf3',
  mkSymmetry,

  SymCode (..),
  symDecode,
  sym16Codes,
  sym16,
  sym16',
  sym48Codes,
  sym48,
  sym48',
  composeSym,
  invertSym,

  -- * Random cube/move
  randomCube,

  -- * Move algebra
  BasicMove,
  oppositeAndGT,

  ElemMove,
  Move,

  reduceMove,
  nubMove,

  -- ** Conversions
  moveToCube,

  moveToString,
  stringToMove,
  ) where

import Rubik.Cube.Moves.Internal
