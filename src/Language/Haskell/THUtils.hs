{- | Convenient definitions to write Template Haskell. -}
module THUtils where

import Data.Function

import Language.Haskell.TH

infixr 0 $$, -$$, $$-, -$$-

-- | @appE@.
($$) = appE

-- | @appE@ where the first argument is a variable @Name@.
(-$$) = appE . varE

-- | @appE@ where the second argument is a variable @Name@.
($$-) = (. varE) . appE

-- | @appE@ where both arguments are variable @Name@s
(-$$-) = appE `on` varE

