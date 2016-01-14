{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Rubik.Cube
import Rubik.Misc
import Rubik.Solver
-- import Rubik.Solver.Optimal
import Rubik.Solver.TwoPhase
import Rubik.Tables.Distances

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Criterion.Measurement ( getCPUTime, secs )

import Data.Binary.Store
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

import Options.Applicative hiding ( value )
import qualified Options.Applicative as Opt

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

data Solver = Optimal | TwoPhase

data Parameters = Parameters {
    verbose :: Bool,
    -- precompute :: Bool,
    solver :: Solver,
    tablePath :: FilePath,
    solverF :: Cube -> Move
  }

type P = ReaderT Parameters

-- | Fill last field
parameters :: Parameters -> Parameters
parameters Parameters{..} =
  let solverF = case solver of
        Optimal -> undefined
        TwoPhase -> twoPhase
  -- when precompute $ compact (phase1PL >> phase2PL) tablePath >> exitSuccess
  -- solverF <- solverPreload tablePath
  in Parameters{..}

optparse :: Parser Parameters
optparse = fmap parameters $ Parameters
  <$> switch (long "verbose" <> short 'v')
  -- <*> switch ( long "precompute" <> short 'p'
  --           <> help "Precompute and store tables" )
  <*> flag TwoPhase Optimal ( long "optimal"
                           <> help "Use optimal solver (experimental)" )
  <*> strOption ( long "table-dir"
               <> metavar "DIR" <> showDefault <> Opt.value ".27"
               <> help "Location of precomputed tables" )
  <*> pure undefined

main :: IO ()
main = do
  evaluate dSym_CornerOrien_FlipUDSlicePermu
  exitSuccess
  p <- execParser $ info (helper <*> optparse) briefDesc
  catchIOError
    (forever $
      runReaderT (answer =<< filter (not . isSpace) <$> lift getLine) p)
    (\e -> if isEOFError e then return () else ioError e)

answer :: String -> P IO ()
answer s = case s of
  '.' : s' -> lift $ moveSequence s'
  "random" -> lift $ putStrLn =<< stringOfCubeColors <$> randomCube
  "quit" -> lift $ exitSuccess
  "" -> return ()
  _ -> faceletList s

-- A sequence of moves, e.g., "URF".
moveSequence s = putStrLn $
  case stringToMove s of
    Left c -> "Unexpected '" ++ [c] ++ "'."
    Right ms -> stringOfCubeColors . moveToCube . reduceMove $ ms

faceletList = either (lift . putStrLn) justSolve . readCube

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

justSolve c = do
  solve <- solverF <$> ask
  let solved = solve c
      solStr = moveToString solved
  vPutStrLn . secs =<< lift (clock $ evaluate solved)
  if c <> moveToCube solved == iden
  then lift $ putStrLn solStr
  else fail $ "Incorrect solver: " ++ solStr

unlessQuiet' :: IO () -> P IO ()
unlessQuiet' a = unlessQuiet (const a) ()

-- Strict in its second argument
unlessQuiet :: (a -> IO ()) -> a -> P IO ()
unlessQuiet f a = do
  v <- verbose <$> ask
  lift $ do
    evaluate a
    when v (f a)

clock :: IO a -> IO Double
clock a = do
  t <- getCPUTime
  a
  t' <- getCPUTime
  return $ t' - t

listSeq' :: [a] -> [a]
listSeq' s = s `listSeq` s

vPutStrLn :: String -> P IO ()
vPutStrLn s = unlessQuiet putStrLn (listSeq' s)

vPutStr :: String -> P IO ()
vPutStr s = unlessQuiet putStrLn (listSeq' s)

