{- Benchmarking script
 -
 - Run with no argument, solves a single random cube.
 - Run with a single "-", initializes and waits for a integer (default to 1
 - if none) and solves that many cubes successively.
 -
 - The cubes are solved with the two phase algorithm using different
 - combinations of implementations,
 - and times are printed for phase 1, phase 2, and the whole.
 -}

import Coord
import Cubie
import Moves
import TwoPhase

import Control.Applicative
import Control.Monad

import Criterion.Measurement

import Data.Monoid

import System.Environment
import System.Random
import System.IO
import System.IO.Error

main = do
  initializeTime
  t <- getTime
  t' <- twoPhaseTables `seq` getTime
  putStrLn $ "Initialized in " ++ secs (t' - t)
  s <- getArgs
  case s of
    ["-"] ->
      catchIOError
        (forever $ do
          putStr "?"
          hFlush stdout
          r <- reads <$> getLine
          let n = case r of
                [] -> 1
                (m, _) : _ -> m
          replicateM n $ randomCube >>= compareSolve)
        (\e -> if isEOFError e then return () else ioError e)
    [] -> randomCube >>= compareSolve
    _ -> return ()

compareSolve c = do
  putStrLn $ "Solve: " ++ stringOfCubeColors c
  t <- getTime
  twoPhase' phase1  phase2  c
  t' <- getTime
  when (t' - t < 3) $ do
    twoPhase' phase1' phase2' c
    -- Crossing phase optimization.
    -- The results of both phases should be the same,
    -- but we leave these to check that everything's alright.
    twoPhase' phase1  phase2' c
    twoPhase' phase1' phase2  c

twoPhase' :: (Cube -> Move) -> (Cube -> Move) -> Cube -> IO ()
twoPhase' p1 p2 c
  = let s1 = p1 c
        c1 = c <> moveToCube s1
        s2 = p2 c1
    in do
      t0 <- getTime
      t1 <- s1 `seq` getTime
      putStrLn . secs $ t1 - t0
      t2 <- s2 `seq` getTime
      putStrLn . secs $ t2 - t1
      putStrLn . secs $ t2 - t0
      putStrLn . moveToString . reduceMove $ s1 ++ s2

intToCube n1 n2 n3 n4 = Cube (Corner cp co) (Edge ep eo)
  where
    cp = let cCP = coordCornerPermu in decode cCP $ mod n1 (range cCP)
    co = let cCO = coordCornerOrien in decode cCO $ mod n2 (range cCO)
    ep = let cEP = coordEdgePermu in decode cEP $ mod n3 (range cEP)
    eo = let cEO = coordEdgeOrien in decode cEO $ mod n4 (range cEO)

randomCube :: IO Cube
randomCube = do
  [n1, n2, n3, n4] <- replicateM 4 randomIO
  let c = intToCube n1 n2 n3 n4
  if solvable c
    then return c
    else randomCube -- proba 1/2

