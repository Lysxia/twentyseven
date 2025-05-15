{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Rubik.Cube
import Rubik.Misc
import qualified Rubik.Solver.Optimal as Optimal
import qualified Rubik.Solver.TwoPhase as TwoPhase
import qualified Rubik.Tables.Internal as Option

import Control.Exception
import Control.Monad

import Data.Time.Clock

import Data.Char
import Data.Monoid

import Numeric ( showFFloat )

import Options.Applicative hiding ( value )

import System.Exit
import System.IO.Error

type Solver = Cube -> Move

data Parameters = Parameters {
    solve :: Solver,
    precompute :: Bool,
    strict :: Bool,
    overwrite :: Bool,
    noFiles :: Bool,
    timed :: Bool,
    tsPath :: Maybe FilePath,
    debug :: Bool
  }

optparse :: Parser Parameters
optparse = Parameters
  <$> flag TwoPhase.solve Optimal.solve ( long "optimal"
        <> help "Use optimal solver (slow!)" )
  <*> switch ( long "precompute" <> short 'p'
        <> help "Precompute and store tables \
                \(do enable this at the first invocation)" )
  <*> switch ( long "strict"
        <> help "Force loading tables before doing anything else" )
  <*> switch ( long "overwrite"
        <> help "Recompute and overwrite tables even when they exist already" )
  <*> switch ( long "no-files"
        <> help "Do not read or write any files \
                \(recompute tables for this session)" )
  <*> switch ( long "timed" <> short 't'
        <> help "Print time taken to solve every cube" )
  <*> (optional . strOption) ( long "ts-dir" <> short 'd'
        <> metavar "DIR"
        <> help "Location of precomputed tables" )
  <*> switch ( long "debug" )

main :: IO ()
main = do
  p <- execParser $ info (helper <*> optparse) briefDesc
  setOptions p
  catchIOError
    (forever $
      flip answer p =<< filter (not . isSpace) <$> getLine)
    (\e -> if isEOFError e then return () else ioError e)

setOptions :: Parameters -> IO ()
setOptions Parameters{..} = do
  mapM_ Option.setTsPath tsPath
  Option.setPrecompute precompute
  Option.setOverwrite overwrite
  Option.setNoFiles noFiles
  Option.setDebug debug
  when strict . void $ evaluate
    (solve . either undefined moveToCube . stringToMove $ "ulfrbd")

answer :: String -> Parameters -> IO ()
answer s p = case s of
  '.' : s' -> moveSequence s'
  "random" -> putStrLn =<< stringOfCubeColors <$> randomCube
  "quit" -> exitSuccess
  "" -> return ()
  _ -> faceletList s p

-- A sequence of moves, e.g., "URF".
moveSequence :: String -> IO ()
moveSequence s = putStrLn $
  case stringToMove s of
    Left c -> "Unexpected '" ++ [c] ++ "'."
    Right ms -> stringOfCubeColors . moveToCube . reduceMove $ ms

faceletList :: [Char] -> Parameters -> IO ()
faceletList = either (const . putStrLn) justSolve . readCube

readCube :: (Eq b, Show b) => [b] -> Either String Cube
readCube s
  = case colorFacelets'' s of
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

justSolve :: Cube -> Parameters -> IO ()
justSolve c p = do
  let solved = solve p c
      solStr = moveToString solved
  time <- clock (evaluate (solved `seq` ()))
  when (timed p) (putStrLn (showFFloat (Just 2) time "s"))
  if c <> moveToCube solved == iden
  then putStrLn solStr
  else fail $ "Incorrect solver: " ++ solStr

clock :: IO () -> IO Double
clock a = do
  t <- getCurrentTime
  a
  t' <- getCurrentTime
  return (diffTimeToSeconds (diffUTCTime t' t))
  where
    diffTimeToSeconds = fromRational . toRational
