{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards,
    ScopedTypeVariables, ViewPatterns #-}
module Rubik.Tables.Internal where

import Rubik.Cube.Cubie
import Rubik.Cube.Coord
import Rubik.Cube.Moves
import Rubik.Distances
import Rubik.Misc
import Rubik.Solver
import Rubik.Symmetry
import Control.Exception
import Control.DeepSeq
import Control.Newtype
import Data.Coerce
import Data.Primitive
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Pinned as P
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Exit

import Debug.Trace

{-# NOINLINE tsPath #-}
tsPath :: IORef FilePath
tsPath = unsafePerformIO (newIORef ".27")

{-# NOINLINE overwrite #-}
overwrite :: IORef Bool
overwrite = unsafePerformIO (newIORef False)

{-# NOINLINE noFiles #-}
noFiles :: IORef Bool
noFiles = unsafePerformIO (newIORef False)

{-# NOINLINE debug #-}
debug :: IORef Bool
debug = unsafePerformIO (newIORef False)

setTsPath :: FilePath -> IO ()
setTsPath = writeIORef tsPath

setTsPathFromHome :: FilePath -> IO ()
setTsPathFromHome p = do
  home <- getHomeDirectory
  setTsPath (home </> p)

setOverwrite :: Bool -> IO ()
setOverwrite = writeIORef overwrite

setNoFiles :: Bool -> IO ()
setNoFiles = writeIORef noFiles

setDebug :: Bool -> IO ()
setDebug = writeIORef debug

data Binary a = Binary
  { encodeFile :: FilePath -> a -> IO ()
  , decodeFile :: FilePath -> IO a
  }

binaryVector :: Prim a => Int -> Binary (P.Vector a)
binaryVector n = Binary
  { encodeFile = P.writeVectorFile
  , decodeFile = \file -> P.readVectorFile file n
  }

binaryVectorList k n = Binary
  { encodeFile = P.writeVectorListFile
  , decodeFile = \file -> P.readVectorListFile file k n
  }

{-# NOINLINE saved #-}
saved :: Binary a -> FilePath -> a -> a
saved binary f a = unsafePerformIO $ do
  noFiles <- readIORef noFiles
  if noFiles then return a else preload binary f a

preload Binary{..} f a = do
  tsPath <- readIORef tsPath
  createDirectoryIfMissing True tsPath
  let path = tsPath </> f
  putStrLn <- bool (\_ -> return ()) putStrLn <$> readIORef debug
  fileExists <- doesFileExist path
  overwrite <- readIORef overwrite
  putStrLn $ ">" ++ f
  a' <- if overwrite || not fileExists then do
      putStrLn ("!" ++ f)
      evaluate a
      encodeFile path a
      return a
    else decodeFile path
  putStrLn $ "<" ++ f
  return a'

saved' :: NFData a => Binary a -> FilePath -> a -> a
saved' b f = saved b f . force

savedVector :: (NFData a, Prim a) => Int -> FilePath -> P.Vector a -> P.Vector a
savedVector = saved' . binaryVector

savedVector_ :: (Coercible a (P.Vector Int)) => FilePath -> a -> a
savedVector_ f a = trace (f ++ show n) $
    coerce $ saved (binaryVector n) f a'
  where
    a' = coerce a :: P.Vector Int
    n = P.length a'

savedVector' :: (Coercible a (P.Vector Int)) => Int -> FilePath -> a -> a
savedVector' n f a = coerce $ saved (binaryVector n) f a'
  where
    a' = coerce a :: P.Vector Int

savedVectorList_ :: (Coercible a [P.Vector Int]) => FilePath -> a -> a
savedVectorList_ f a = trace (f ++ show (k, n)) $
    coerce $ saved (binaryVectorList k n) f a'
  where
    a' = coerce a :: [P.Vector Int]
    k = length a'
    n = P.length (head a')

savedVectorList' :: (Coercible a [P.Vector Int]) => Int -> Int -> FilePath -> a -> a
savedVectorList' k n f a = coerce $ saved (binaryVectorList k n) f a'
  where
    a' = coerce a :: [P.Vector Int]

savedVectorList :: (NFData a, Prim a)
  => Int -> Int -> FilePath -> [P.Vector a] -> [P.Vector a]
savedVectorList = saved' .: binaryVectorList
  where (.:) = (.) (.) (.)

rawMoveTables :: (CubeAction a, RawEncodable a)
  => MoveTag m [Cube] -> MoveTag m [RawMove a]
rawMoveTables moves = (over MoveTag . fmap) moveTable moves

savedRawMoveTables
  :: forall a m. (CubeAction a, RawEncodable a)
  => String -> MoveTag m [Cube] -> MoveTag m [RawMove a]
savedRawMoveTables name moves@(MoveTag moves')
  = savedVectorList' (length moves') (range ([] :: [a])) name
      (rawMoveTables moves)

rawSymTables :: RawEncodable a
  => (Cube -> a -> a) -> [Symmetry sym] -> MoveTag sym [RawMove a]
rawSymTables conj syms = MoveTag $ symTable conj <$> symAsCube <$> syms

savedRawSymTables :: forall a sym. RawEncodable a
  => String -> (Cube -> a -> a) -> [Symmetry sym]
  -> MoveTag sym [RawMove a]
savedRawSymTables name conj syms
  = savedVectorList' (length syms) (range ([] :: [a])) name
      (rawSymTables conj syms)

move18to10 :: MoveTag Move18 [as] -> MoveTag Move10 [as]
move18to10 (MoveTag as) = MoveTag
  (composeList as [ n - 1 + 3 * fromEnum m | (n, m) <- unMoveTag move10Names ])

distanceTable2 :: (FromCube a, FromCube b, RawEncodable a, RawEncodable b)
  => String -> MoveTag m [RawMove a] -> MoveTag m [RawMove b]
  -> P.Vector DInt
distanceTable2 name m1 m2
  = savedVector (n1 * n2) name (distanceWith2' m1 m2 proj1 proj2 n1 n2)
  where
    proj1 = rawProjection
    proj2 = rawProjection
    n1 = range (proxyUnwrap proj1)
    n2 = range (proxyUnwrap proj2)

distanceWith2'
  :: MoveTag m [RawMove a] -> MoveTag m [RawMove b]
  -> Projection' m a -> Projection' m b -> Int -> Int -> P.Vector DInt
distanceWith2' (MoveTag m1) (MoveTag m2) proj1 proj2 n1 n2 = distances n root neighbors
  where
    n = n1 * n2
    root = flatIndex n2 (unRawCoord (convertP proj1 iden)) (unRawCoord (convertP proj2 iden))
    neighbors ((`divMod` n2) -> (x1, x2))
      = zipWith (\v1 v2 -> flatIndex n2
          (unRawCoord . indexP proj1 v1 $ RawCoord x1)
          (unRawCoord . indexP proj2 v2 $ RawCoord x2)) m1 m2

indexWithSym
  :: MoveTag sym (V.Vector (RawMove b))
  -- Conjugation by the inverse: s <> b <> s^-1
  -> Int
  -> SymCoord sym a
  -> RawCoord b
  -> Int
indexWithSym sb nb (SymClass xa, i) xb = flatIndex nb xa (symB sb i xb)
  where
    symB :: MoveTag sym (V.Vector (RawMove b)) -> SymCode sym -> RawCoord b -> Int
    symB (MoveTag s) (SymCode i) (RawCoord xb) = unRawMove (s V.! i) P.! xb

distanceWithSym2'
  :: MoveTag m [SymMove sym a] -> MoveTag m [RawMove b]
  -> MoveTag sym (V.Vector (RawMove b))
  -> SymProjection m sym a
  -> Projection' m b
  -> Int
  -> Int
  -> P.Vector DInt
distanceWithSym2' (MoveTag ma) (MoveTag mb) sb a b na nb
  = distances n root neighbors
  where
    n = na * nb
    root = flatIndex nb (unSymClass . fst $ convertP a iden) (unRawCoord (convertP b iden))
    neighbors ((`divMod` nb) -> (xa, xb))
      = zipWith (\va vb ->
          let ya = indexP a va (SymClass xa, SymCode 0 :: SymCode sym)
              yb = indexP b vb (RawCoord xb)
          in indexWithSym sb nb ya yb) ma mb

castDistance :: Distance m (RawCoord a) -> Distance m (RawCoord (Symmetric sym a))
castDistance = coerce
