import Misc
import Coord
import Cubie
import Facelet
import Moves
import TwoPhase

import Control.Applicative
import Control.Monad
import Control.DeepSeq

import Data.List
import Data.Maybe

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

main :: IO ()
main = do
  arg <- filter (/= ' ') <$> concat <$> getArgs
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

answer s = do
  let
    ans =
      case s of
        '.' : s' -> moveSequence s'
        _ -> faceletList s
  case ans of
    Left err -> putStrLn err
    Right c -> putStrLn c

-- A sequence of moves, e.g., "URF".
moveSequence s =
  case mapM decodeMove s of
    Nothing -> Left "Expected string of \"ulfrbd\" after a dot."
    Just cs -> Right . stringOfCubeColors $ foldl' (?) iden cs

faceletList s =
  case normalize s of
    Nothing -> Left "Expected string of length 54 of a set of (any) 6 characters. Centers must be distinct."
    Just colors ->
      case colorFaceletsToCube colors of
        Left fs -> Left $ "Facelets " ++ show fs ++ " (" ++ show (map (s !!) fs) ++ ") do not match any regular cubie."
        Right Nothing -> Left $ "Not a permutation of cubies (a cubie is absent, and a cubie occurs twice)."
        Right (Just c) | solvable c -> Right . intercalate " " . map snd . fromJust $ twoPhase c
        _ -> Left $ "Unsolvable cube."

