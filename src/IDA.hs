{- |
 - Implementation of the IDA* search algorithm
 -}
{-# LANGUAGE ScopedTypeVariables #-}
module IDA where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List
import qualified Data.Set as S

-- | Type of outgoing edges, labelled and weighted.
data Succ label length node
  = Succ {
      eLabel :: label,
      eCost :: length,
      eSucc :: node
    }

-- | IDA* search
--
-- All paths to goal(s) are returned, grouped by length.
--
-- Only searches as deep as necessary thanks to lazy evaluation.
--
-- TODO: Possible memory leak, solving hard cubes eats a lot of memory.
search
  :: forall l a node
   . (Num a, Ord a)
  => node                      -- ^ root
  -> (node -> Bool)            -- ^ goal predicate
  -> (node -> a)               -- ^ underestimate of distance to goal
  -> (node -> [Succ l a node]) -- ^ labeled successors
  -> [(a, [[l]])]
search root goal h succ
  = rootSearch (h root)
  where
    -- Search from the root up to a distance @d@
    -- for increasing values of @d@.
    rootSearch :: a -> [(a, [[l]])]
    rootSearch d =
      let (ds, found) = search' root 0 [] d
          deepen = maybe [] rootSearch ds
      in (d, found) : deepen

    -- Depth-first search up to depth @bound@,
    -- and list all results at the leaves.
    search' :: node -> a -> [l] -> a -> (Maybe a, [[l]])
    search' n g ls bound
      | g == bound && isGoal = (Nothing, [reverse ls])
      | f > bound            = (Just f, [])
      | otherwise
      = let (as', ls) = unzip . map searchSucc $ succ n
        in case catMaybes as' of
            [] -> (Nothing, concat ls)
            as -> (Just (minimum as), concat ls)
      where
        isGoal = goal n
        f = g + h n
        searchSucc (Succ eLabel eCost eSucc)
          = search' eSucc (g + eCost) (eLabel : ls) bound

-- | Filter search output
nonEmpty :: [(a, [[l]])] -> [(a, [[l]])]
nonEmpty = filter (not . null . snd)

-- | First result
first :: [(a, [[l]])] -> Maybe (a, [[l]])
first = find (not . null . snd)

-- | Record wrapping search parameters
data GraphSearch l a node = GS {
  root :: node,
  goal :: node -> Bool,
  estm :: node -> a,
  succs :: node -> [Succ l a node] }

-- | wrapped 'search'
search' :: (Num a, Ord a) => GraphSearch l a node -> [(a, [[l]])]
search' (GS root goal estm succ)
  = search root goal estm succ

-- | Search while avoiding self-intersection.
--
-- The use of 'Set' requires an 'Ord' instance
search''
  :: forall l a node
   . (Num a, Ord a, Ord node) => GraphSearch l a node -> [(a, [[l]])]
search'' gs = search' gs'
  where
    gs' :: GraphSearch l a (node, S.Set node)
    gs' = GS {
      root = (root gs, S.singleton $ root gs),
      goal = goal gs . fst,
      estm = estm gs . fst,
      succs = succs' }
    succs' :: (node, S.Set node) -> [Succ l a (node, S.Set node)]
    succs' (n, visited) =
      let ns = filter ((`S.notMember` visited) . eSucc) . succs gs $ n
          annot n'@(Succ l c s) = Succ {
            eLabel = l,
            eCost = c,
            eSucc = (s, S.insert s visited) }
      in map annot ns

