import IDA

import System.Random
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M

type Point = Int
type Dim = (Int, Int)
type BiMap a = M.Map a [a] -- Symmetric map

data MazeState = MS {
  dim :: Dim,
  edges :: BiMap Point,
  stack :: [Point] }

bmInsert :: Ord a => a -> a -> BiMap a -> BiMap a
bmInsert a b = insert' a b . insert' b a
  where
    insert' a b = M.alter (Just . maybe [b] (b :)) a

adjacent :: Dim -> Point -> [Point]
adjacent dim@(m, n) p
  = map (toPoint dim)
  . filter withinBounds
  $ [(i+1,j), (i-1,j), (i,j+1), (i, j-1)]
  where
    (i,j) = fromPoint dim p
    withinBounds (i, j) = 0 <= i && i < m && 0 <= j && j < n

toPoint :: Dim -> (Int, Int) -> Point
toPoint (_, n) (i, j) = i * n + j

fromPoint :: Dim -> Point -> (Int, Int)
fromPoint (m, n) p = (p `div` n, p `mod` n)

startMaze :: Int -> Int -> IO MazeState
startMaze m n = do
  pt <- randomRIO (0, m * n - 1)
  return $
    MS {
      dim = (m, n),
      edges = M.empty,
      stack = [pt] }

-- Assuming not (null stack)
mazeStep :: MazeState -> IO MazeState
mazeStep ms = do
  let pt = head (stack ms)
      adj = filter (`M.notMember` edges ms) (adjacent (dim ms) pt)
  if null adj
  then return $ ms { stack = tail (stack ms) }
  else do
    pt' <- (adj !!) <$> randomRIO (0, length adj - 1)
    return $
      ms {
        edges = bmInsert pt pt' $ edges ms,
        stack = pt' : stack ms }

doUntil :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
doUntil stop init step = go init
  where go init | stop init = return init
                | otherwise = step init >>= go

mazeGen :: Int -> Int -> IO (BiMap Point)
mazeGen m n = do
  init <- startMaze m n
  edges <$> doUntil (null . stack) init mazeStep

mazeGS :: Dim -> Point -> Point -> BiMap Point -> GraphSearch Int Int Point
mazeGS dim root goal m
  = GS {
      root = root,
      goal = (== goal),
      estm = estm,
      succs = succs }
  where
    (gi, gj) = fromPoint dim goal
    estm p = let (i, j) = fromPoint dim p in abs (gi - i) + abs (gj - j)
    succs p = zipWith3 Succ [0 ..] (repeat 1) . M.findWithDefault [] p $ m

mazeTest :: Int -> Int -> IO ()
mazeTest m n = do
  maze <- mazeGen m n
  print maze
  let randPt = randomRIO (0, m * n - 1)
  s <- randPt
  t <- randPt
  print (s, t)
  let gs = mazeGS (m, n) s t maze
      r = search'' gs
  print r
