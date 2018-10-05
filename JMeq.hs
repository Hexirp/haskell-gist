{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module JMeq where
 import Prelude

 import Data.Type.Equality

 eq_JMeq :: a :~~: b -> a :~: b
 eq_JMeq HRefl = Refl
