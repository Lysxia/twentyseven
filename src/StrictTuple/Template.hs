{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module StrictTuple.Template where


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
    decTupleTranspose ] <*> pure n

tupleL :: Int -> [Q Exp] -> Q Exp
tupleL n = appsE . (conE (tupleName n) :)

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
          . tupleL n . replicate n $ varE x) []]
    apD = do
      fs <- replicateM n (newName "f")
      xs <- replicateM n (newName "x")
      let body = normalB . tupleL n $ zipWith (-$$-) fs xs
      funD (mkName "<*>")
        [clause (conP name <$> (varP <$>) <$> [fs, xs]) body []]

decTupleTranspose :: Int -> Q Dec
decTupleTranspose n = do
  x <- replicateM n $ newName "x"
  xs <- replicateM n $ newName "xs"
  funD transpose
    [clause
      [conP name $ zipWith (\(varP -> x') (varP -> xs') -> infixP x' '(:) xs') x xs]
      (normalB $ body x xs)
      []]
  where
    name = tupleName n
    transpose = mkName $ "transpose" ++ show n
    body x xs = uInfixE (tupleVarL x) (conE '(:)) (transpose -$$ tupleVarL xs)
    tupleVarL = tupleL n . (varE <$>)

