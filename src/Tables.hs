{- | Some tables of numbers for fast look up. -}

module Tables where

symClassesFlipUDSlice :: Vector SymCoord
symClassesFlipUDSlice
  = symClasses
      coordFlipUDSlice
      (map conjugateFlipUDSlice sym16)

