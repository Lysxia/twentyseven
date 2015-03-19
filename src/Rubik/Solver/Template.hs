{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Rubik.Solver.Template where

import Rubik.IDA
import Rubik.Misc
import Rubik.Cube

import Control.Applicative
import Control.Monad

import Data.List ( transpose )
import Data.StrictTuple.Template
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Language.Haskell.TH
import Language.Haskell.THUtils

data DistParamQ
  = OneP Name Name
  | TwoP Name {-^ Int variable -} Name Name Name

-- | Body of a search function parameterized by move table names.
searchWithQ
  :: Name -- ^ Move set
  -> Name -- ^ Conversion function (Cube -> coordinate tuple)
  -> [Name] -- ^ Coordinate-wise move tables
  -> [DistParamQ] -- ^ Distance tables, with corresponding move table names
  -> Q Exp
searchWithQ moveID convert moves dists
  = [| Search
      { goal = ($(varE convert) iden ==) . snd,
        estm = $(estmQ moves dists),
        edges = $(edgesQ moveID moves) } |]

estmQ :: [Name] -> [DistParamQ] -> Q Exp
estmQ moves dists = do
    xs <- replicateM n $ newName "x"
    lam1E (tupP [wildP, tupleP $ varP <$> xs]) [| maximum $(listE $ distOf xs <$> dists) |]
  where
    n = length moves
    distOf xs (OneP mv d) =
      case lookup mv $ zip moves xs of
        Nothing -> invalidName mv
        Just x -> [| $(varE d) U.! $(varE x) |]
    distOf xs (TwoP h mv1 mv2 d) =
      let z = zip moves xs in
      case (lookup mv1 z, lookup mv2 z) of
        (Nothing, _) -> invalidName mv1
        (_, Nothing) -> invalidName mv2
        (Just x, Just y) ->
          [| $(varE d) U.! flatIndex $(varE h) $(varE x) $(varE y) |]

edgesQ moveID moves = [|
    let ms = zip $(varE moveID) . map $(fromListE n) $ transpose $(listE' moves)
        succV = V.snoc
          (V.generate 6 $ \(toEnum -> i) ->
            [ m | m@((_, j), _) <- ms,
              not (i == j || oppositeAndGT j i) ])
          ms
    in \(i, t) -> map
        (\(l@(_, j), succs) ->
          let x = (U.!) <$> succs <*> t in x `seq` Succ l 1 (fromEnum j, x))
        (succV V.! i) |]
  where
    n = length moves

-- | Body of the conversion function.
convertQ :: [Name] -> ExpQ
convertQ coord = [| ($(tupleE $ (\c -> [| encode $(varE c) . fromCube |]) <$> coord) <*>) . pure |]

-- | Returns a pair consisting of the conversion function
-- and the @Search@ record.
csQ :: Name -> [(Name, Name)] -> [DistParamQ] -> ExpQ
csQ moveID z dists = [|
    let c = $(convertQ encode)
    in (c, $(searchWithQ moveID 'c moves dists)) |]
  where (encode, moves) = unzip z

