import Rubik.Cube
import Rubik.Misc
import Rubik.Solver
import Rubik.Solver.Optimal

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Criterion.Measurement ( getCPUTime, secs )

import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

data Parameters = Parameters {
    quiet :: Bool,
    preload :: Bool
  }

defaultParam = Parameters {
    quiet = False,
    preload = False
  }

unlessQuiet :: Monad m => m () -> ReaderT Parameters m ()
unlessQuiet a =
  ask >>= \p -> unless (quiet p) (lift a)

-- Strict in its second argument
uQ :: Monad m => (a -> m ()) -> a -> ReaderT Parameters m ()
uQ f a
  = a `seq` unlessQuiet (f a)

type P = ReaderT Parameters

clock :: a -> IO Double
{-# NOINLINE clock #-}
clock x = do
  t <- getCPUTime
  evaluate x
  t' <- getCPUTime
  return $ t' - t

qPutStrLn :: String -> P IO ()
qPutStrLn s = do
  p <- ask
  lift $ if quiet p
    then evaluate (s `listSeq` ())
    else putStrLn s

qPutStr :: String -> P IO ()
qPutStr s = do
  p <- ask
  lift $ if quiet p
    then evaluate (s `listSeq` ())
    else putStr s

doPreload :: P IO ()
{-# NOINLINE doPreload #-}
doPreload = do
  t <- lift $ getCPUTime
  qPutStrLn "Moves."
  clockCI "CornerPermu" move18CornerPermu
  clockCI "CornerOrien" move18CornerOrien
  clockCI "EdgeOrien" move18EdgeOrien
  clockCI "UD/LR/FBSlice" move18UDSlice
  clockCI "UD/LR/FBSlicePermu" move18UDSlicePermu
  qPutStrLn "Distances."
  clockPrint "dist_CornerPermu" dist_CornerPermu
  clockPrint "dist_CornerOrien_UDSlice" dist_CornerOrien_UDSlice
  clockPrint "dist_CornerOrien_LRSlice" dist_CornerOrien_LRSlice
  clockPrint "dist_CornerOrien_FBSlice" dist_CornerOrien_FBSlice
  clockPrint "dist_EdgeOrien_UDSlice" dist_EdgeOrien_UDSlice
  clockPrint "dist_EdgeOrien_LRSlice" dist_EdgeOrien_LRSlice
  clockPrint "dist_EdgeOrien_FBSlice" dist_EdgeOrien_FBSlice
  t' <- lift $ getCPUTime
  qPutStrLn $ "Total: " ++ secs (t' - t)
  where
    clockCI s l = clockPrint s (movesCI l `listSeq` ())
    clockPrint s x = do
      t <- lift $ clock x
      qPutStrLn $ s ++ ": \t" ++ secs t

main :: IO ()
main = do
  (p, args') <- prepareArgs defaultParam
  let arg = filter (/= ' ') . concat $ args'
  when (preload p) $ runReaderT doPreload p
  case arg of
    [] -> do
      catchIOError
        (forever $ do
          s <- filter (/= ' ') <$> getLine
          if s == ""
          then exitSuccess
          else runReaderT (answer s) p)
        (\e -> if isEOFError e then return () else ioError e)
    s -> runReaderT (answer s) p

prepareArgs :: Parameters -> IO (Parameters, [String])
prepareArgs p = prepare p <$> getArgs
  where
    prepare p ("-p" : args) = prepare (p { preload = True }) args
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

justSolve c = do
  lift (clock solve) >>= (qPutStrLn . secs)
  if c <> moveToCube solve == iden
  then lift . putStrLn . moveToString $ solve
  else fail ("Incorrect solver: " ++ moveToString solve)
  where
    solve = optim c

