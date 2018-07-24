{-# LANGUAGE RankNTypes #-}

module Stream where
 import Prelude
 
 import Control.Monad (join)

 newtype Streamly m a =
  Streamly {
   unStreamly :: forall r. m r -> (a -> Streamly m a -> m r) -> m r
  }

 sNil :: Streamly m a
 sNil = Streamly $ \inNil _ -> inNil

 sCons :: a -> Streamly m a -> Streamly m a
 sCons x xs = Streamly $ \_ inCons -> inCons x xs

 sConsM :: Monad m => m a -> Streamly m a -> Streamly m a
 sConsM mx xs = Streamly $ \_ inCons -> join $ inCons <$> mx <*> pure xs

 (|:) :: Monad m => m a -> Streamly m a -> Streamly m a
 (|:) = sConsM

 infixr 5 |:

 instance Semigroup (Streamly m a) where
  x <> y =
   Streamly $ \inNil inCons ->
    unStreamly x (unStreamly y inNil inCons) $ \xv xs ->
     inCons xv (xs <> y)

 instance Monoid (Streamly m a) where
  mempty = sNil

 instance Functor (Streamly m) where
  fmap f x =
   Streamly $ \inNil inCons ->
    unStreamly x inNil $ \xv xs ->
     inCons (f xv) (fmap f xs)

 instance Applicative (Streamly m) where
  pure x = sCons x sNil

  f <*> x =
   Streamly $ \inNil inCons ->
    unStreamly f inNil $ \fv fs ->
     unStreamly (fmap fv x <> (fs <*> x)) inNil inCons

 instance Monad (Streamly m) where
  x >>= f =
   Streamly $ \inNil inCons ->
    unStreamly x inNil $ \xv xs ->
     unStreamly (f xv <> (xs >>= f)) inNil inCons
