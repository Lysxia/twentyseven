{- |
 - Implementation of the IDA search algorithm
 -}
{-# LANGUAGE ScopedTypeVariables #-}
module IDA where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont
import qualified Data.Set as S

-- | Type of outgoing edges,
-- labelled and valued
data Succ l a node
  = Succ {
      eLabel :: l,
      eCost :: a,
      eSucc :: node
    }

data Status a b = Deepen a | Found a b | NotFound

mergeStatus :: Ord a => Status a b -> Status a b -> Status a b
mergeStatus s@(Found _ _)  _ = s
mergeStatus NotFound    s' = s'
mergeStatus _ s'@(Found _ _) = s'
mergeStatus s  NotFound    = s
mergeStatus (Deepen a) (Deepen b) = Deepen (min a b)

statusSeq :: Ord a => [Status a b] -> Status a b
statusSeq = foldr mergeStatus NotFound

-- | IDA search
search
  :: forall l a node
   . (Num a, Ord a)
  => node                      -- ^ root
  -> (node -> Bool)            -- ^ goal predicate
  -> (node -> a)               -- ^ (under)estimate of distance to goal
  -> (node -> [Succ l a node]) -- ^ labeled successors
  -> Maybe (a, [l])
search root goal h succ
  = runCont (rootSearch . h $ root) id
  where
    -- Search from the root up to a distance @d@
    -- for increasing values of @d@.
    rootSearch :: a -> Cont r (Maybe (a, [l]))
    rootSearch d = do
      s <- search' root 0 [] d
      case s of
        Deepen d' -> rootSearch d'
        Found d p -> return $ Just (d, p)
        NotFound  -> return Nothing

    -- Depth-first search up to depth @bound@
    search' :: node -> a -> [l] -> a -> Cont r (Status a [l])
    search' n g ls bound
      | f > bound = return $ Deepen f
      | goal n    = return $ Found g (reverse ls)
      | otherwise = fmap statusSeq . mapM searchSucc . succ $ n
      where
        f = g + h n
        searchSucc (Succ eLabel eCost eSucc)
          = search' eSucc (g + eCost) (eLabel : ls) bound

-- | Record wrapping search parameters
data GraphSearch l a node = GS {
  root :: node,
  goal :: node -> Bool,
  estm :: node -> a,
  succs :: node -> [Succ l a node] }

search' :: (Num a, Ord a) => GraphSearch l a node -> Maybe (a, [l])
search' (GS root goal estm succ)
  = search root goal estm succ

-- | Search while avoiding self-intersection. The use of Set requires an Ord instance
search''
  :: forall l a node
   . (Num a, Ord a, Ord node) => GraphSearch l a node -> Maybe (a, [l])
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

