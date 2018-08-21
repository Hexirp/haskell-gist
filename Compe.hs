module Compe where
 import Prelude

 import Data.Kind (Type)

 data Comparg :: Type -> Type where
  Get :: Read a => Comparg a
  Vec :: (Read a, Integral i) => i -> Comparg [a]
  Str :: Comparg String
