{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Data.StrictTuple.Template where

import Control.Applicative
import Control.Monad
import Data.Function ( on )
import Language.Haskell.TH
import Language.Haskell.THUtils

-- | $(tupleName n) = Tuple[n]
tupleName :: Int -> Name
tupleName n = mkName $ "Tuple" ++ show n

decTuple :: Int -> Q [Dec]
decTuple n = sequence $
  [ decTupleType,
    decTupleInstApplicative,
    decTupleTranspose,
    decTupleFromList ] <*> pure n

tupleE :: [ExpQ] -> ExpQ
tupleE args = appsE $ conE (tupleName n) : args
  where n = length args

tupleP :: [PatQ] -> PatQ
tupleP ps = conP (tupleName n) ps
  where n = length ps

transposeE :: Int -> ExpQ
transposeE n = varE . mkName $ "transpose" ++ show n

fromListE :: Int -> ExpQ
fromListE n = varE . mkName $ "fromList" ++ show n

decTupleType :: Int -> Q Dec
decTupleType n = do
  a <- newName "a"
  dataD (cxt []) name [PlainTV a] [con a] deriv
  where
    name = tupleName n
    con a = normalC name . replicate n $ strictType isStrict (varT a)
    deriv = mkName <$> ["Eq", "Foldable", "Functor", "Show"]

decTupleInstApplicative :: Int -> Q Dec
decTupleInstApplicative n =
  instanceD (cxt []) (conT (mkName "Applicative") `appT` conT name) [pureD, apD]
  where
    name = tupleName n
    pureD = do
      x <- newName "x"
      funD (mkName "pure")
        [clause [varP x] (normalB
          . tupleE . replicate n $ varE x) []]
    apD = do
      fs <- replicateM n (newName "f")
      xs <- replicateM n (newName "x")
      let body = normalB . tupleE $ zipWith (-$$-) fs xs
      funD (mkName "<*>")
        [clause (conP name <$> (varP <$>) <$> [fs, xs]) body []]

decTupleTranspose :: Int -> Q Dec
decTupleTranspose n = do
  x <- replicateM n $ newName "x"
  xs <- replicateM n $ newName "xs"
  funD transpose
    [clause
      [tupleP $ zipWith (\(varP -> x') (varP -> xs') -> [p| $x' : $xs' |]) x xs]
      (normalB $ [| $(tupleE' x) : $(transposeE n) $(tupleE' xs) |])
      []]
  where
    transpose = mkName $ "transpose" ++ show n
    tupleE' = tupleE . (varE <$>)

decTupleFromList :: Int -> Q Dec
decTupleFromList n = do
  xs <- replicateM n $ newName "xs"
  funD fromList
    [clause
      [listP $ varP <$> xs]
      (normalB $ [| $(tupleE' xs) |])
      []]
  where
    fromList = mkName $ "fromList" ++ show n
    tupleE' = tupleE . (varE <$>)
