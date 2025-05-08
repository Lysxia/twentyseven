{-# LANGUAGE CPP, TemplateHaskell, ViewPatterns #-}
module Data.Tuple.Template where

import Control.Monad
import Language.Haskell.TH hiding ( tupleT )

-- | $(tupleName n) = Tuple[n]
tupleName :: Int -> Name
tupleName n = mkName $ "(" ++ replicate (n-1) ',' ++ ")"

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

decTupleCons :: Int -> Q Dec
decTupleCons n = do
  aas@(a : as) <- replicateM n (varT <$> newName "a")
  instanceD (cxt [])
    (foldl appT (conT (mkName "TupleCons"))
      [tupleT as])
    [typeD aas, consInlD, consD, splitInlD, splitD]
  where
    typeD aas@(a : as) =
#if MIN_VERSION_template_haskell(2, 15, 0)
      TySynInstD <$> tySynEqn Nothing (conT (mkName ":|") `appT` a `appT` tupleT as) (tupleT aas)
#else
      TySynInstD (mkName ":|") <$> tySynEqn [a, tupleT as] (tupleT aas)
#endif
    consInlD = pragInlD (mkName "|:|") Inline FunLike AllPhases
    splitInlD = pragInlD (mkName "split") Inline FunLike AllPhases
    consD = do
      xxs@(x : xs) <- replicateM n (newName "x")
      funD (mkName "|:|")
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
