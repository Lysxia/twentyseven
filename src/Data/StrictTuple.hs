{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
  TemplateHaskell, DeriveTraversable, ViewPatterns #-}
module Data.StrictTuple where

import Control.Monad ( forM )
import Data.StrictTuple.Template ( decTuple )

class TupleCons a b c | a b -> c, c -> a b where
  (|*|) :: a -> b -> c
  split :: c -> (a, b)

concat <$> forM [1 .. 10] decTuple
