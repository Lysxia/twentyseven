module Moves
  where

import Cubie

-- Elementary moves

u =
  cube ([1, 2, 3, 0] ++ [4..7])
       (replicate 8 0)
       ([1, 2, 3, 0] ++ [4..11])
       (replicate 12 0)

-- Symmetries

surf3 =
  cube [4, 5, 2, 1, 6, 3, 0, 7]
       [2, 1, 2, 1, 2, 1, 2, 1]
       [5, 9, 1, 8, 7, 11, 3, 10, 6, 2, 4, 0]
       [1, 0, 1, 0, 1,  0, 1,  0, 1, 1, 1, 1]

sf2 =
  cube [6, 5, 4, 7, 2, 1, 0, 3]
       (replicate 8 0)
       [6, 5, 4, 7, 2, 1, 0, 3, 9, 8, 11, 10]
       (replicate 12 0)

su4 =
  cube [1, 2, 3, 0, 5, 6, 7, 4]
       (replicate 8 0)
       [1, 2, 3, 0, 5, 6, 7, 4, 9, 11, 8, 10]
       (replicate 8 0 ++ [1, 1, 1, 1])

slr2 =
  cube [3, 2, 1, 0, 5, 4, 7, 6]
       (replicate 8 3)
       [2, 1, 0, 3, 6, 5, 4, 7, 9, 8, 11, 10]
       (replicate 12 0)

