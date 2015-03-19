{-# LANGUAGE NamedFieldPuns #-}

import Rubik.Cube
import Rubik.Misc
import Rubik.Solver
import Rubik.Solver.Optimal
import Rubik.Solver.TwoPhase

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

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

data Solver = Optimal | TwoPhase

data Parameters = Parameters {
    verbose :: Bool,
    precompute :: Bool,
    solver :: Solver,
    tablePath :: FilePath,
    solverF :: Cube -> Move
  }

type P = ReaderT Parameters

defaultParam = Parameters {
    verbose = False,
    precompute = False,
    solver = TwoPhase,
    tablePath = ".27",
    solverF = undefined
  }

main :: IO ()
main = do
  p <- prepareArgs defaultParam
  catchIOError
    (forever $
      runReaderT (answer =<< filter (not . isSpace) <$> lift getLine) p)
    (\e -> if isEOFError e then return () else ioError e)

prepareArgs :: Parameters -> IO Parameters
prepareArgs p' = do
    p <- prepare p' <$> getArgs
    when (precompute p) $ runReaderT doPreload p >> exitSuccess
    solverF <- preloadFrom (tablePath p) $ case solver p of
          Optimal -> optim
          TwoPhase -> twoPhase
    return $ p { solverF }
  where
    prepare p args' = case args' of
      "-p" : args -> prepare (p { precompute = True }) args
      "-v" : args -> prepare (p { verbose = True }) args
      "--optimal" : args -> prepare (p { solver = Optimal }) args
      "--twophase" : args -> prepare (p { solver = TwoPhase {} }) args
      z : args | "--path=" `isPrefixOf` z ->
        prepare (p { tablePath = drop 7 z }) args
      a : _ -> error $ "Unrecognized option: " ++ a
      [] -> p

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
  lift $ if v
    then f a
    else evaluate a >> return ()

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

doPreload :: P IO ()
doPreload = do
  s <- solver <$> ask
  case s of
    Optimal -> doPreloadOptimal
    TwoPhase -> doPreloadTwoPhase

doPreloadOptimal = do
  t <- lift $ getCPUTime
  vPutStrLn "Moves."
  clockCI "CornerPermu" move18CornerPermu
  clockCI "CornerOrien" move18CornerOrien
  clockCI "EdgeOrien" move18EdgeOrien
  clockCI "UD/LR/FBSlice" move18UDSlice
  clockCI "UD/LR/FBSlicePermu" move18UDSlicePermu
  vPutStrLn "Distances."
  clockPrint "dist_CornerPermu" dist_CornerPermu
  clockStore "dist_CornerOrien_UDSlice" dist_CornerOrien_UDSlice
  clockStore "dist_CornerOrien_LRSlice" dist_CornerOrien_LRSlice
  clockStore "dist_CornerOrien_FBSlice" dist_CornerOrien_FBSlice
  clockStore "dist_EdgeOrien_UDSlice" dist_EdgeOrien_UDSlice
  clockStore "dist_EdgeOrien_LRSlice" dist_EdgeOrien_LRSlice
  clockStore "dist_EdgeOrien_FBSlice" dist_EdgeOrien_FBSlice
  vPutStrLn "Save."
  save <- (mapM . store . tablePath) <$> ask
  lift $ save optimDistTables
  t' <- lift $ getCPUTime
  vPutStrLn $ "Total: " ++ secs (t' - t)
  where
    clockCI s l = clockPrint s (movesCI l `listSeq` ())
    clockStore s t = clockPrint s (value t `seq` ())
    clockPrint s x = do
      t <- lift $ clock (evaluate x)
      vPutStrLn $ s ++ ": \t" ++ secs t

doPreloadTwoPhase = do
  t <- lift $ getCPUTime
  vPutStrLn "Moves."
  clockCI "CornerPermu" move18CornerPermu
  clockCI "CornerOrien" move18CornerOrien
  clockCI "EdgeOrien" move18EdgeOrien
  clockCI "UDSlice" move18UDSlice
  vPutStrLn "Distances."
  clockStore "dist_CornerOrien_UDSlice" dist_CornerOrien_UDSlice
  clockStore "dist_EdgeOrien_UDSlice" dist_EdgeOrien_UDSlice
  clockStore "dist_CornerPermu_UDSlicePermu2" dist_CornerPermu_UDSlicePermu2
  clockStore "dist_UDEdgePermu2" dist_EdgePermu2
  vPutStrLn "Save."
  save <- (mapM . store . tablePath) <$> ask
  lift $ save phase1DistTables
  lift $ save phase2DistTables
  t' <- lift $ getCPUTime
  vPutStrLn $ "Total: " ++ secs (t' - t)
  where
    clockCI s l = clockPrint s (movesCI l `listSeq` ())
    clockStore s t = clockPrint s (value t `seq` ())
    clockPrint s x = do
      t <- lift $ clock (evaluate x)
      vPutStrLn $ s ++ ": \t" ++ secs t

