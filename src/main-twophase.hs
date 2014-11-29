import Misc
import Coord
import Cubie
import Facelet
import Moves
import TwoPhase

import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

main :: IO ()
main = do
  arg <- filter (/= ' ') . concat <$> (prepare =<< getArgs)
  case arg of
    [] -> do
      catchIOError
        (forever $ do
          putStr "> "
          hFlush stdout
          s <- filter (/= ' ') <$> getLine
          if s == ""
          then exitSuccess
          else answer s)
        (\e -> if isEOFError e then return () else ioError e)
    s -> answer s
  where
    -- Option "-": precompute tables before interaction
    prepare ("-" : args) = twoPhaseTables `seq` return args
    prepare args = return args

answer s = do
  case s of
    '.' : s' -> moveSequence s'
    "!" -> randomCube >>= justSolve
    _ -> faceletList s

-- A sequence of moves, e.g., "URF".
moveSequence s =
  putStrLn $
    case stringToMove s of
      Left c -> "Unexpected '" ++ [c] ++ "'."
      Right ms -> stringOfCubeColors . moveToCube . reduceMove $ ms

faceletList = either putStrLn justSolve . readCube

readCube s =
  case normalize s of
    Nothing -> Left "Expected string of length 54 of a set of (any) 6 \
                    \characters. Centers must be distinct."
    Just colors ->
      case colorFaceletsToCube colors of
        Left fs ->
          Left $ "Facelets " ++ show fs
              ++ " (" ++ show (map (s !!) fs) ++ ") \
                 \do not match any regular cubie."
        Right Nothing ->
          Left "Not a permutation of cubies \
               \(a cubie is absent, and a cubie occurs twice)."
        Right (Just c) | solvable c -> Right c
        _ -> Left "Unsolvable cube."

justSolve c =
  putStrLn . moveToString . twoPhase $ c

