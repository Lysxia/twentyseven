{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TemplateHaskell, DeriveFoldable, DeriveFunctor, ViewPatterns #-}
module Data.StrictTuple where

import Control.Applicative ( Applicative (..), (<$>) )
import Control.Monad ( forM )

import Data.Foldable ( Foldable )
import Data.StrictTuple.Template ( decTuple )

import Language.Haskell.TH

$(concat <$> forM [2 .. 10] decTuple)

