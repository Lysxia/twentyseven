module Moves (
  -- ** Generating moves
  u,r,f,d,l,b,
  move6,
  decodeMove,
  decodeMoveSequence,

  -- ** 18 elementary moves
  move18Names,
  move18,
 
  -- ** Other subgroups
  move6',
  move10Names,
  move10,

  -- ** Symmetries
  surf3, sf2, su4, slr2,
  symCode,
  sym16,
  sym48,
  ) where

import Coord
import Cubie
import Misc
import Symmetry

import Control.Applicative

import Data.Char ( toLower )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

move18Names, move10Names :: [String]
move18Names = [[f,n] | f <- "ULFRBD", n <- " 2'"]
move10Names = [[f,n] | f <- "UD", n <- " 2'"] ++ [[f,'2'] | f <- "LFRB"]

-- Elementary moves

u_ =
  unsafeCube' ([1, 2, 3, 0] ++ [4..7])
          (replicate 8 0)
          ([1, 2, 3, 0] ++ [4..11])
          (replicate 12 0)

u  = u_
r  = surf3 ?? u
f  = surf3 ?? r
d  = sf2   ?? u
l  = surf3 ?? d
b  = surf3 ?? l

decodeMove :: Char -> Maybe Cube
decodeMove = (`lookup` zip "ulfrbd" move6) . toLower

decodeMoveSequence :: String -> Either Char [Cube]
decodeMoveSequence [] = return []
decodeMoveSequence (m : ms) = do
  c <- maybe (Left m) Right $ decodeMove m
  case ms of
    '\'' : next -> (c ?^ 3 :) <$> decodeMoveSequence next
    '2' : next -> (c ?^ 2 :) <$> decodeMoveSequence next
    _ -> (c :) <$> decodeMoveSequence ms

-- | List of the 6 generating moves.
--
-- > move6 = [u,l,f,r,b,d]
move6  = [u, l, f, r, b, d]

-- | List of the 18 elementary moves.
--
-- > move18 = [u, u ?^ 2, u ?^ 3, ...]
move18 = move6 >>= \x -> [x, x ?^ 2, x ?^ 3]

-- | > G1 = <U, D, L2, F2, R2, B2>
move6' = [u,d] ++ map (?^ 2) [l, f, r, b]
move10 = ([u, d] >>= \x -> [x, x ?^ 2, x ?^ 3]) ++ drop 2 move6'

-- Symmetries

surf3 =
  unsafeCube' [4, 5, 2, 1, 6, 3, 0, 7]
          [2, 1, 2, 1, 2, 1, 2, 1]
          [5, 9, 1, 8, 7, 11, 3, 10, 6, 2, 4, 0]
          [1, 0, 1, 0, 1,  0, 1,  0, 1, 1, 1, 1]

sf2 =
  unsafeCube' [6, 5, 4, 7, 2, 1, 0, 3]
          (replicate 8 0)
          [6, 5, 4, 7, 2, 1, 0, 3, 9, 8, 11, 10]
          (replicate 12 0)

su4 =
  unsafeCube' [1, 2, 3, 0, 5, 6, 7, 4]
          (replicate 8 0)
          [1, 2, 3, 0, 5, 6, 7, 4, 9, 11, 8, 10]
          (replicate 8 0 ++ [1, 1, 1, 1])

slr2 =
  unsafeCube' [3, 2, 1, 0, 5, 4, 7, 6]
          (replicate 8 6)
          [2, 1, 0, 3, 6, 5, 4, 7, 9, 8, 11, 10]
          (replicate 12 0)

-- x <- [0..47]
-- 2 * 4 * 2 * 3 = 48
-- 2 * 4 * 2 = 16
symCode :: Coord -> Cube
symCode = (es V.!)
  where es = V.generate 47 eSym'
        eSym' x = (Moves.surf3 ?^ x1)
                ? (Moves.sf2   ?^ x2)
                ? (Moves.su4   ?^ x3)
                ? (Moves.slr2  ?^ x4)
          where x4 =  x          `mod` 2
                x3 = (x `div` 2) `mod` 4
                x2 = (x `div` 8) `mod` 2
                x1 =  x `div` 16 -- < 3

sym16 = map symCode [0..15]
sym48 = map symCode [0..47]

