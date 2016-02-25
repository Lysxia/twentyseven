{-# LANGUAGE TypeFamilies, TypeOperators, TemplateHaskell #-}
module Data.Tuple.Extra where

import Control.Monad ( forM )
import Data.Tuple.Template ( decTupleCons )

class TupleCons b where
  type (:|) a b :: *
  (|:|) :: a -> b -> a :| b
  split :: a :| b -> (a, b)

forM [3 .. 10] decTupleCons

newtype Tuple1 a = Tuple1 a
  deriving (Eq, Ord, Show)

instance TupleCons (Tuple1 a) where
   type (:|) b (Tuple1 a) = (b, a)
   a |:| Tuple1 b = (a, b)
   split (a, b) = (a, Tuple1 b)
