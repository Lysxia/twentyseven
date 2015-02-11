{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TemplateHaskell, DeriveFoldable, DeriveFunctor, ViewPatterns #-}
module StrictTuple where

import StrictTuple.Template ( decTuple )

import Control.Applicative ( Applicative (..), (<$>) )
import Control.Monad ( forM )

import Data.Foldable ( Foldable )

import Language.Haskell.TH

$(concat <$> forM [2 .. 10] decTuple)

