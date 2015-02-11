{- |
 - Implementation of the IDA* search algorithm
 -}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Rubik.IDA where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List
import qualified Data.Set as S

-- | Type of outgoing edges, labelled and weighted.
data Succ label length node = Succ {
    eLabel :: label,
    eCost :: length,
    eSucc :: node
  }

data Search a l node = Search {
    goal :: node -> Bool,
    estm :: node -> a,
    edges :: node -> [Succ l a node]
  }

type Result a l = [(a, [[l]])]

-- | Depth-first search up to depth @bound@,
-- and reduce results from the leaves.
dfSearch
  :: (Num a, Ord a)
  => Search a l node
  -> node -> a -> [l] -> a -> (Maybe a, [[l]])
{-# INLINE dfSearch #-}
dfSearch (Search goal estm edges) n g ls bound
  = dfs n g ls bound
  where
    dfs n g ls bound
      | g == bound && goal n = (Nothing, [reverse ls])
      | f > bound            = (Just f, [])
      | otherwise
      = let (as', ls) = unzip . map searchSucc $ edges n
            a' = case catMaybes as' of
              [] -> Nothing
              as -> Just (minimum as)
            in (a', concat ls)
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
  :: forall a l node . (Num a, Ord a)
  => Search a l node
  -> node {- ^ root -} -> Result a l
{-# INLINE search #-}
search s root = rootSearch (estm s root)
  where
    -- Search from the root up to a distance @d@
    -- for increasing values of @d@.
    rootSearch :: a -> [(a, [[l]])]
    rootSearch d =
      let (d', result) = dfSearch s root 0 [] d
          deepen = maybe [] rootSearch d'
      in case result of
        [] -> deepen
        r -> (d, r) : deepen

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

