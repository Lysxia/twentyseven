module Coord
  where

import Data.Array.Unboxed

type Coord = Int

encode :: Int -> UArray Int Int -> Int
encode base a = foldl1 ((+).(* base)) $ elems a


