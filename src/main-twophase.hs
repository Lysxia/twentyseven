import Misc
import Coord
import Cubie
import Facelet
import Moves
import TwoPhase

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.DeepSeq

import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

data Parameters = Parameters {
    quiet :: Bool
  }

defaultParam = Parameters {
    quiet = False
  }

unlessQuiet :: Monad m => m () -> ReaderT Parameters m ()
unlessQuiet a =
  ask >>= \p -> unless (quiet p) (lift a)

-- Strict in its second argument
uQ :: Monad m => (a -> m ()) -> a -> ReaderT Parameters m ()
uQ f a
  = a `seq` unlessQuiet (f a)

main :: IO ()
main = do
  (p, args') <- prepareArgs defaultParam
  let arg = filter (/= ' ') . concat $ args'
  case arg of
    [] -> do
      catchIOError
        (forever $ do
          unless (quiet p) (putStr "> " >> hFlush stdout)
          s <- filter (/= ' ') <$> getLine
          if s == ""
          then exitSuccess
          else runReaderT (answer s) p)
        (\e -> if isEOFError e then return () else ioError e)
    s -> runReaderT (answer s) p
  where

prepareArgs :: Parameters -> IO (Parameters, [String])
prepareArgs p = prepare p <$> getArgs
  where
    -- Option "-": precompute tables before interaction
    prepare p ("-" : args) = twoPhaseTables `seq` prepare p args
    prepare p ("-q" : args) = prepare (p { quiet = True }) args
    prepare p args = (p, args)

answer :: String -> ReaderT Parameters IO ()
answer s = do
  case s of
    '.' : s' -> uQ moveSequence s'
    "random" -> uQ putStrLn =<< stringOfCubeColors <$> lift randomCube
    "solverandom" -> do
      c <- lift randomCube
      uQ putStrLn (stringOfCubeColors c)
      justSolve c
    _ -> faceletList s

-- A sequence of moves, e.g., "URF".
moveSequence s = putStrLn $
  case stringToMove s of
    Left c -> "Unexpected '" ++ [c] ++ "'."
    Right ms -> stringOfCubeColors . moveToCube . reduceMove $ ms

faceletList = either (unlessQuiet . putStrLn) justSolve . readCube

readCube s
  = case normalize s of
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

justSolve c
  = let sol = twoPhase c
    in uQ putStrLn . moveToString $ sol

