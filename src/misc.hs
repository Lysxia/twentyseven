module Misc where

import Data.Array.Unboxed

-- Lists

rotate n l = l2 ++ l1
  where (l1, l2) = splitAt n l

subs 0 x (a : as) = x : as
subs n x (a : as) = a : subs (n-1) x as
subs _ _ []       = undefined

-- Arrays

idArray r = listArray r $ range r

composeArray a b = ixmap (bounds b) (b !) a

class Group a where
  id :: a
  compose :: a -> a -> a

instance (Group a, Group b) => Group (a, b) where
  id = (Misc.id, Misc.id)
  (a1, b1) `compose` (a2, b2) = (a1 `compose` a2, b1 `compose` b2)
