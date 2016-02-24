{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Rubik.Cube
import Rubik.Misc
-- import Rubik.Solver.Optimal
import Rubik.Solver.TwoPhase

import Control.Exception
import Control.Monad

import Criterion.Measurement ( getCPUTime, secs )

import Data.Char
import Data.Monoid

import Options.Applicative hiding ( value )
import qualified Options.Applicative as Opt

import System.Exit
import System.IO.Error

type Solver = Cube -> Move

data Parameters = Parameters {
    verbose :: Bool,
    -- precompute :: Bool,
    solver :: Solver,
    tablePath :: FilePath
  }

optparse :: Parser Parameters
optparse = Parameters
  <$> switch (long "verbose" <> short 'v')
  -- <*> switch ( long "precompute" <> short 'p'
  --           <> help "Precompute and store tables" )
  <*> flag twoPhase (\_ -> undefined) ( long "optimal"
                           <> help "Use optimal solver (experimental)" )
  <*> strOption ( long "table-dir"
               <> metavar "DIR" <> showDefault <> Opt.value ".27"
               <> help "Location of precomputed tables" )

main :: IO ()
main = do
  p <- execParser $ info (helper <*> optparse) briefDesc
  catchIOError
    (forever $
      flip answer p =<< filter (not . isSpace) <$> getLine)
    (\e -> if isEOFError e then return () else ioError e)

answer :: String -> Parameters -> IO ()
answer s p = case s of
  '.' : s' -> moveSequence s'
  "random" -> putStrLn =<< stringOfCubeColors <$> randomCube
  "quit" -> exitSuccess
  "" -> return ()
  _ -> faceletList s p

-- A sequence of moves, e.g., "URF".
moveSequence s = putStrLn $
  case stringToMove s of
    Left c -> "Unexpected '" ++ [c] ++ "'."
    Right ms -> stringOfCubeColors . moveToCube . reduceMove $ ms

faceletList = either (const . putStrLn) justSolve . readCube

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
  let solved = solver p c
      solStr = moveToString solved
  flip vPutStrLn p . secs =<< clock (evaluate solved)
  if c <> moveToCube solved == iden
  then putStrLn solStr
  else fail $ "Incorrect solver: " ++ solStr

unlessQuiet' :: IO () -> Parameters -> IO ()
unlessQuiet' a = unlessQuiet (const a) ()

-- Strict in its second argument
unlessQuiet :: (a -> IO ()) -> a -> Parameters -> IO ()
unlessQuiet f a p = evaluate a >> when (verbose p) (f a)

clock :: IO a -> IO Double
clock a = do
  t <- getCPUTime
  a
  t' <- getCPUTime
  return $ t' - t

listSeq' :: [a] -> [a]
listSeq' s = s `listSeq` s

vPutStrLn :: String -> Parameters -> IO ()
vPutStrLn s = unlessQuiet putStrLn (listSeq' s)

vPutStr :: String -> Parameters -> IO ()
vPutStr s = unlessQuiet putStrLn (listSeq' s)
