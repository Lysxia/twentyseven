{- |
 - Implementation of the IDA* search algorithm
 -}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Rubik.IDA where

import qualified Data.Set as S

-- | Type of outgoing edges, labelled and weighted.
data Succ label length node = Succ {
    eLabel :: label,
    eCost :: length,
    eSucc :: node
  }

data Search f a l node = Search {
    goal :: node -> Bool,
    estm :: node -> a,
    edges :: node -> f (Succ l a node)
  }

type Result a l = Maybe [l]
data SearchResult a l = Next !a | Found [l] | Stop

instance Ord a => Monoid (SearchResult a l) where
  {-# INLINE mempty #-}
  mempty = Stop
  {-# INLINE mappend #-}
  mappend f@(Found _) _ = f
  mappend _ f@(Found _) = f
  mappend (Next a) (Next b) = Next (min a b)
  mappend Stop x = x
  mappend x Stop = x

-- | Depth-first search up to depth @bound@,
-- and reduce results from the leaves.
dfSearch
  :: (Foldable f, Num a, Ord a)
  => Search f a l node
  -> node -> a -> [l] -> a -> SearchResult a l
{-# INLINE dfSearch #-}
dfSearch (Search goal estm edges) n g ls bound
  = dfs n g ls bound
  where
    dfs n g ls bound
      | g == bound && g == f && goal n = Found (reverse ls)
      | f > bound = Next f
      | otherwise
      = foldMap searchSucc $ edges n
      where
        isGoal = goal n
        f = g + estm n
        searchSucc (Succ eLabel eCost eSucc)
          = dfs eSucc (g + eCost) (eLabel : ls) bound

-- | IDA* search
--
-- All paths to goal(s) are returned, grouped by length.
--
-- Only searches as deep as necessary thanks to lazy evaluation.
--
-- TODO: Possible memory leak, solving hard cubes eats a lot of memory.
search
  :: forall f a l node . (Foldable f, Num a, Ord a)
  => Search f a l node
  -> node {- ^ root -} -> Maybe [l]
{-# INLINE search #-}
search s root = rootSearch (estm s root)
  where
    -- Search from the root up to a distance @d@
    -- for increasing values of @d@.
    rootSearch :: a -> Maybe [l]
    rootSearch d =
      case dfSearch s root 0 [] d of
        Stop -> Nothing
        Found ls -> Just ls
        Next d' -> rootSearch d'

data SelfAvoid node = SelfAvoid (S.Set node) node

selfAvoid (Search goal estm edges) = Search {
    goal = goal . node,
    estm = estm . node,
    edges = edges'
  }
  where
    node (SelfAvoid _ n) = n
    edges' (SelfAvoid trace n)
      = [ Succ l c (SelfAvoid (S.insert s trace) s)
        | Succ l c s <- edges n, S.notMember s trace ]

selfAvoidRoot root = (root, S.singleton root)

