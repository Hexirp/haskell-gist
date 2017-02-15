{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Untypeclass where

-- ################################

type Semigroup m = m -> m -> m

instance_semigroup_int_by_addition :: Semigroup Int
instance_semigroup_int_by_addition = (+)

instance_semigroup_int_by_multiplication :: Semigroup Int
instance_semigroup_int_by_multiplication = (*)

-- ################################

data Functor' :: (* -> *) -> * where
  Functor'
    :: (forall a b. (a -> b) -> f a -> f b)
    -> Functor' f

instance_functor_natural :: Functor f => Functor' f
instance_functor_natural = Functor' fmap

fmap' :: Functor' f -> (a -> b) -> f a -> f b
fmap' (Functor' f) = f

-- ################################

data Applicative' :: (* -> *) -> *  where
  Applicative'
    :: Functor' f
    -> (forall a. a -> f a)
    -> (forall a b. f (a -> b) -> f a -> f b)
    -> Applicative' f

applicatve2
  :: Functor' f
  -> (forall a. a -> f a)
  -> (forall a b c. (a -> b -> c) -> f a -> f b -> f c)
  -> Applicative' f
applicatve2 fun pure app = Applicative' fun pure $ app ($)

fapp' :: Applicative' f -> f (a -> b) -> f a -> f b
fapp' (Applicative' _ _ f) = f

instance_applicative_natural :: Applicative f => Applicative' f
instance_applicative_natural = Applicative' (Functor' fmap) pure (<*>)

instance_applicative_ziplist :: Applicative' []
instance_applicative_ziplist = applicatve2 (Functor' map) repeat zipWith

-- ################################

main :: IO ()
main = do
  return ()