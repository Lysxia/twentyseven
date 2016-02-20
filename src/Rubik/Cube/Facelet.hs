{- |
   Facelet representation

   Facelets faces are unfolded and laid out like this:

   @
       U
     L F R B
       D
   @

   Faces (or colors) are ordered @U, L, F, R, B, D@.

   A Rubik's cube is a permutation of facelets numbered as follows:

   >            0  1  2
   >            3  4  5
   >            6  7  8
   >
   >  9 10 11  18 19 20  27 28 29  36 37 38
   > 12 13 14  21 22 23  30 31 32  39 40 41
   > 15 16 17  24 25 26  33 34 35  42 43 44
   >
   >           45 46 47
   >           48 49 50
   >           51 52 53

-}

module Rubik.Cube.Facelet (
  -- * Facelet permutation
  numFacelets,
  Facelets,
  facelets,
  fromFacelets,

  -- * Colors
  Color,
  colorOf,
  colorChar,

  -- * Color list
  ColorFacelets,
  colorFacelets,
  fromColorFacelets,
  colorFaceletsOf,

  -- * List conversions
  fromFacelets',
  facelets',
  fromColorFacelets',
  colorFacelets',
  colorFacelets'',

  -- * Pretty conversion
  stringOfFacelets,
  stringOfColorFacelets,
  stringOfColorFacelets',

  -- * Facelets corresponding to each cubie

  -- | The first letter in the name of a cubie is
  -- the color of its reference facelet
  -- (illustrated at @http://kociemba.org/math/cubielevel.htm@).
  --
  -- Corner colors are given in clockwise order.
  --
  -- Corners are lexicographically ordered
  -- (@U>L>F>R>B>D@).
  --
  -- Edges are gathered by horizontal slices (@U, D, UD@).

  -- ** Centers
  centerFacelets,

  -- ** Corners
  cornerFacelets,
  ulb, ufl, urf, ubr, dlf, dfr, drb, dbl,

  -- ** Edges
  edgeFacelets,
  ul, uf, ur, ub, dl, df, dr, db, fl, fr, bl, br
  ) where

import Rubik.Cube.Facelet.Internal
