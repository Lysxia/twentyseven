{- |
 - Implementation of the IDA* search algorithm
 -}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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

class (Num a, Ord a) => Searchable a l node | node -> a l where
  goal :: node -> Bool
  estm :: node -> a
  edges :: node -> [Succ l a node]

-- | Depth-first search up to depth @bound@,
-- and reduce results from the leaves.
{-# INLINABLE dfSearch #-}
dfSearch :: Searchable a l node => node -> a -> [l] -> a -> (Maybe a, [[l]])
dfSearch n g ls bound
  | g == bound && goal n = (Nothing, [reverse ls])
  | f > bound            = (Just f, [])
  | otherwise
  = let (as', ls) = unzip . map searchSucc $ edges n
    in case catMaybes as' of
        [] -> (Nothing, concat ls)
        as -> (Just (minimum as), concat ls)
  where
    isGoal = goal n
    f = g + estm n
    searchSucc (Succ eLabel eCost eSucc)
      = dfSearch eSucc (g + eCost) (eLabel : ls) bound

-- | IDA* search
--
-- All paths to goal(s) are returned, grouped by length.
--
-- Only searches as deep as necessary thanks to lazy evaluation.
--
-- TODO: Possible memory leak, solving hard cubes eats a lot of memory.
search
  :: forall a l node . Searchable a l node
  => node {- ^ root -} -> [(a, [[l]])]
{-# INLINABLE search #-}
search root = rootSearch (estm root)
  where
    -- Search from the root up to a distance @d@
    -- for increasing values of @d@.
    rootSearch :: a -> [(a, [[l]])]
    rootSearch d =
      let (d', found) = dfSearch root 0 [] d
          deepen = maybe [] rootSearch d'
      in (d, found) : deepen

-- | Filter search output
nonEmpty :: [(a, [[l]])] -> [(a, [[l]])]
nonEmpty = filter (not . null . snd)

-- | First result
first :: [(a, [[l]])] -> Maybe (a, [[l]])
first = find (not . null . snd)

data SelfAvoid node = SelfAvoid node (S.Set node)

-- | Search while avoiding self-intersection.
--
-- The use of 'Set' requires an 'Ord' instance
instance (Searchable a l node, Ord node) => Searchable a l (SelfAvoid node) where
  goal (SelfAvoid n _) = goal n
  estm (SelfAvoid n _) = estm n
  edges (SelfAvoid n visited) =
    let annot n'@(Succ l c s) = Succ {
          eCost = c,
          eLabel = l,
          eSucc = SelfAvoid s (S.insert s visited) }
    in map annot . filter ((`S.notMember` visited) . eSucc) . edges $ n

selfAvoidRoot root = (root, S.singleton root)

