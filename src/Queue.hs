module Queue (
  Queue,
  empty,
  singleton,
  push,
  append,
  pop,
  view ) where

data Queue a = [a] :> [a]

empty :: Queue a
empty = [] :> []

singleton :: a -> Queue a
singleton x = [] :> [x]

push :: a -> Queue a -> Queue a
push x (l :> r) = (x : l) :> r

view :: Queue a -> Maybe (a, Queue a)
view ([] :> []) = Nothing
view (l :> []) = view ([] :> reverse l)
view (l :> (top : r)) = Just (top, l :> r)

pop :: Queue a -> Maybe a
pop = fmap fst . view

append :: [a] -> Queue a -> Queue a
append xs (l :> r) = ((xs ++ l) :> r)

