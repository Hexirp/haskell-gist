{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- ################################

data Semigroup :: * -> * where
  Semigroup
    :: (m -> m -> m)
    -> Semigroup m

instance_semigroup_int_by_addition :: Semigroup Int
instance_semigroup_int_by_addition = Semigroup (+)

instance_semigroup_int_by_multiplication :: Semigroup Int
instance_semigroup_int_by_multiplication = Semigroup (*)

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
instance_applicative_ziplist = applicatve2 (Functor' map) (:[]) zipWith

-- ################################

main :: IO ()
main = do
  let f = (+0):(+1):(+2):[]
  let x = 0:1:2:3:4:[]
  print $ fmap' instance_functor_natural (+ 1) (Just 1) -- Just 2
  print $ fapp' instance_applicative_natural f x        -- [0,1,2,3,4,1,2,3,4,5,2,3,4,5,6]
  print $ fapp' instance_applicative_ziplist f x        -- [0,2,4]