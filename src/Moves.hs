{-# LANGUAGE ViewPatterns #-}
module Moves (
  -- ** Generating moves
  u,r,f,d,l,b,
  move6,

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

  -- ** 
  Move,

  stringToMove,
  
  concatMoves,
  moveToString,
  moveToCube,
  ) where

import Coord
import Cubie
import Misc
import Symmetry

import Control.Applicative

import Data.Char ( toLower )
import Data.Function ( on )
import Data.List
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

move18Names, move10Names :: [Move]
move18Names = [Move (replicate n m) | m <- [U .. D], n <- [1 .. 3]]
move10Names = [Move (replicate n m) | m <- [U, D], n <- [1 .. 3]] ++ [Move [m, m] | m <- [L .. B]]

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

--

-- | Minimal set of moves
data BasicMove = U | L | F | R | B | D
  deriving (Enum, Eq, Ord, Show, Read)

newtype Move = Move { fromMove :: [BasicMove] }

instance Monoid Move where
  mempty = Move []
  mappend = (Move .) . concatMove `on` fromMove

instance Group Move where
  inverse = reduceToMove . (>>= replicate 3) . reverse . fromMove

oppositeAndLT :: BasicMove -> BasicMove -> Bool
oppositeAndLT = curry (`elem` [(U, D), (L, R), (F, B)])

infixr 5 `consMove`

consMove :: BasicMove -> [BasicMove] -> [BasicMove]
consMove m [] = [m]
consMove m (span (== m) -> (ms, ns)) | length ms == 3 = ns
consMove m (n : ns) | oppositeAndLT m n = n `consMove` m `consMove` ns
consMove m ns = m : ns

concatMove :: [BasicMove] -> [BasicMove] -> [BasicMove]
concatMove = flip (foldr consMove)

concatMoves :: [Move] -> Move
concatMoves = foldr (?) iden

reduceToMove :: [BasicMove] -> Move
reduceToMove = Move . (`concatMove` [])

moveToString :: Move -> String
moveToString = intercalate " " . (map $ \ms@(m : _) -> show m ++
  case length ms of
    1 -> ""
    2 -> "2"
    3 -> "'") . group . fromMove

moveToCube :: Move -> Cube
moveToCube = moveToCube' . fromMove

moveToCube' :: [BasicMove] -> Cube
moveToCube' [] = iden
moveToCube' (m : ms) = basicMoveToCube m ? moveToCube' ms

basicMoveToCube :: BasicMove -> Cube
basicMoveToCube = (move6 !!) . fromEnum

-- | Associates s character in @"ULFRBD"@ or the same in lowercase
-- to a generating move.
decodeMove :: Char -> Maybe BasicMove
decodeMove = (`lookup` zip "ulfrbd" [U .. D]) . toLower

-- | Reads a space-free sequence of moves.
-- If the string is incorrectly formatted,
-- the first wrong character is returned.
--
-- @([ulfrbd][23']?)*@
stringToMove :: String -> Either Char Move
stringToMove [] = return iden
stringToMove (x : xs) = do
  m <- maybe (Left x) Right $ decodeMove x
  case xs of
    o : next | o `elem` ['\'', '3'] -> (Move [m, m, m] ?) <$> stringToMove next
    '2' : next -> (Move [m, m] ?) <$> stringToMove next
    _ -> (Move [m] ?) <$> stringToMove xs

