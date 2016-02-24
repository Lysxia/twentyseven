{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  TemplateHaskell, DeriveTraversable, TypeFamilies, TypeOperators,
  ViewPatterns #-}
module Data.StrictTuple where

import Control.Monad ( forM )
import Data.StrictTuple.Template ( decTuple )

class TupleCons b where
  type (:|) a b :: *
  (|:|) :: a -> b -> a :| b
  split :: (a :| b) -> (a, b)

concat <$> forM [1 .. 15] decTuple
