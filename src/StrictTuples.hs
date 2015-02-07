{-# LANGUAGE TemplateHaskell #-}
module StrictTuples where

import Control.Applicative
import Control.Monad
import Data.Function ( on )
import Language.Haskell.TH

decTuple :: Int -> Q [Dec]
decTuple n = sequence [decTupleType n, decTupleInstApplicative n]

decTupleType :: Int -> Q Dec
decTupleType n = do
  a <- newName "a"
  return $ DataD [] name [PlainTV a] [con a] deriv
  where
    name = tupleName n
    con a = NormalC name $ replicate n (IsStrict, VarT a)
    deriv = mkName <$> ["Eq", "Foldable", "Functor", "Show"]

tupleName :: Int -> Name
tupleName n = mkName $ "Tuple" ++ show n

decTupleInstApplicative :: Int -> Q Dec
decTupleInstApplicative n = do
  pureD <- pureD'
  apD <- apD'
  return $ InstanceD [] (AppT (ConT (mkName "Applicative")) (ConT name)) [pureD, apD]
  where
    name = tupleName n
    pureD' = do
      x <- newName "x"
      return $ FunD (mkName "pure")
        [Clause [VarP x] (NormalB . listToTuple . replicate n $ VarE x) []]
    apD' = do
      fs <- replicateM n (newName "f")
      xs <- replicateM n (newName "x")
      let body = NormalB . listToTuple $ zipWith (AppE `on` VarE) fs xs
      return $ FunD (mkName "<*>")
        [Clause [ConP name (VarP <$> fs), ConP name (VarP <$> xs)] body []]

listToTuple :: [Exp] -> Exp
listToTuple l = foldl AppE (ConE (tupleName n)) l
  where n = length l

