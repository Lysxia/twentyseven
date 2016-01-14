{-# LANGUAGE LambdaCase, ScopedTypeVariables, ViewPatterns, TemplateHaskell #-}
module Rubik.Tables.Internal where

import Rubik.Cube.Cubie
import Rubik.Cube.Coord
import Rubik.Cube.Moves
import Rubik.Distances
import Rubik.Misc
import Rubik.Solver
import Rubik.Symmetry
import Control.Monad
import qualified Data.Binary as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Coerce
import Data.Int ( Int8 )
import Data.FileEmbed
import Data.Typeable
import qualified Data.Vector.Unboxed as U
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO.Unsafe
import Debug.Trace

binaryDecode :: B.Binary a => BS.ByteString -> a
binaryDecode = B.decode . BL.fromStrict

fp = (".27/" ++)

embed :: FilePath -> BS.ByteString -> Q Exp
embed (fp -> file) s = do
  runIO $ do
    fileExists <- doesFileExist file
    unless fileExists $ BS.writeFile file (BS.pack "" `const` s)
  embedFile file

saved :: B.Binary a => FilePath -> a -> a
saved (fp -> f) a = unsafePerformIO $ do
  fileExists <- doesFileExist f
  if fileExists
  then B.encodeFile f a >> return a
  else B.decodeFile f `catchIOError` \_ -> do
    B.encodeFile f a
    return a

thTypeOf :: Typeable a => proxy a -> Q Type
thTypeOf = thTypeOf' . typeRep

thTypeOf' :: TypeRep -> Q Type
thTypeOf' (splitTyConApp -> (con, args)) = foldl appT con' (fmap thTypeOf' args)
  where
    con' = 
      case tyConName con of
        "[]" -> listT
        "(,)" -> tupleT 2
        "(,,)" -> tupleT 3
        n -> lookupTypeName n >>= \case
          Just a -> conT a
          Nothing -> fail $ "Type name " ++ n ++ " not found."

embedBinary :: (B.Binary a, Typeable a) => FilePath -> a -> Q Exp
embedBinary file a = sigE [| binaryDecode $(embed file . BL.toStrict $ B.encode a) |] (thTypeOf [a])

rawMoveTables :: CubeAction a => MoveTag m [Cube] -> RawEncoding a -> MoveTag m [RawMove a]
rawMoveTables (MoveTag moves) enc = MoveTag $ moveTable enc <$> moves

embedRawMoveTables
  :: (Typeable a, Typeable m, CubeAction a)
  => String -> MoveTag m [Cube] -> RawEncoding a -> Q Exp
embedRawMoveTables name moves enc = embedBinary name (rawMoveTables moves enc)
savedRawMoveTables name moves enc = saved name (rawMoveTables moves enc)

rawSymTables :: (Cube -> a -> a) -> [Symmetry sym] -> RawEncoding a -> MoveTag sym [RawMove a]
rawSymTables conj syms enc = MoveTag $ symTable conj enc <$> symAsCube <$> syms

embedRawSymTables name conj syms enc = embedBinary name (rawSymTables conj syms enc)
savedRawSymTables name conj syms enc = saved name (rawSymTables conj syms enc)

move18to10 :: MoveTag Move18 [as] -> MoveTag Move10 [as]
move18to10 (MoveTag as) = MoveTag
  (composeList as [ n - 1 + 3 * fromEnum m | (n, m) <- unMoveTag move10Names ])

distanceTable2
  :: String -> MoveTag m [RawMove a] -> MoveTag m [RawMove b]
  -> Projection' m a -> Projection' m b -> RawEncoding a -> RawEncoding b
  -> {- Q Exp -} Vector DInt
distanceTable2 name m1 m2 proj1 proj2 (range -> n1) (range -> n2)
  = saved name (distanceWith2' m1 m2 proj1 proj2 n1 n2)

distanceWith2'
  :: MoveTag m [RawMove a] -> MoveTag m [RawMove b]
  -> Projection' m a -> Projection' m b -> Int -> Int -> Vector DInt
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
    symB (MoveTag s) (SymCode i) (RawCoord xb) = unRawMove (s V.! i) U.! xb

distanceWithSym2'
  :: MoveTag m [SymMove sym a] -> MoveTag m [RawMove b]
  -> MoveTag sym (V.Vector (RawMove b))
  -> SymProjection m sym a
  -> Projection' m b
  -> Int
  -> Int
  -> Vector DInt
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

tagOf :: tag a b -> tag' a b' -> tag a b
tagOf = const

embedSymMoveTables :: forall a m sym
  . (Typeable m, Typeable sym, Typeable a)
  => String -> MoveTag m [Cube] -> RawEncoding a
  -> Action sym a -> SymClassTable sym a -> (Cube -> a -> a)
  -> Q Exp -- MoveTag m [SymMove sym a]
embedSymMoveTables name (MoveTag moves) enc action classes conj
  = embedBinary name (MoveTag [ symMoveTable enc action classes (conj c) | c <- moves ] :: MoveTag m [SymMove sym a])
