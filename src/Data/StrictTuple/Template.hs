{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Data.StrictTuple.Template where

import Control.Applicative
import Control.Monad
import Data.Function ( on )
import Language.Haskell.TH hiding ( tupleT )
import Language.Haskell.THUtils

-- | $(tupleName n) = Tuple[n]
tupleName :: Int -> Name
tupleName n = mkName $ "Tuple" ++ show n

decTuple :: Int -> Q [Dec]
decTuple n = sequence $
  [ decTupleType,
    decTupleTranspose,
    decTupleFromList ]
  ++ [ decTupleCons | n > 1 ] <*> pure n

tupleT :: [TypeQ] -> TypeQ
tupleT args = appsT (conT (tupleName n)) args
  where
    appsT = foldl appT 
    n = length args

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
decTupleType 1 = fmap head [d|newtype Tuple1 a = Tuple1 a deriving (Eq, Show)|]
decTupleType n = do
  as <- replicateM n (newName "a")
  dataD (cxt []) name (fmap PlainTV as) [con as] deriv
  where
    name = tupleName n
    con = normalC name . fmap (strictType isStrict . varT)
    deriv = mkName <$> ["Eq", "Show"]

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

decTupleCons :: Int -> Q Dec
decTupleCons n = do
  aas@(a : as) <- replicateM n (varT <$> newName "a")
  instanceD (cxt [])
    (foldl appT (conT (mkName "TupleCons"))
      [a, tupleT as, tupleT aas])
    [consD, splitD]
  where
    consD = do
      xxs@(x : xs) <- replicateM n (newName "x")
      funD (mkName "|*|")
        [ clause
          [ varP x, tupleP (varP <$> xs) ]
          (normalB [| $(tupleE' xxs) |])
          [] ]
    splitD = do
      xxs@(x : xs) <- replicateM n (newName "x")
      funD (mkName "split")
        [ clause
          [ tupleP (varP <$> xxs) ]
          (normalB [| ($(varE x), $(tupleE' xs)) |])
          [] ]
    tupleE' = tupleE . fmap varE
