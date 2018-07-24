{-# LANGUAGE RankNTypes #-}

module Stream where
 import Prelude
 
 import Control.Monad (join)

 newtype Stream m a =
  Stream {
   unStream :: forall r. m r -> (a -> Stream m a -> m r) -> m r
  }

 sNil :: Stream m a
 sNil = Stream $ \inNil _ -> inNil

 sCons :: a -> Stream m a -> Stream m a
 sCons x xs = Stream $ \_ inCons -> inCons x xs

 sConsM :: Monad m => m a -> Stream m a -> Stream m a
 sConsM mx xs = Stream $ \_ inCons -> join $ inCons <$> mx <*> pure xs

 (|:) :: Monad m => m a -> Stream m a -> Stream m a
 (|:) = sConsM

 infixr 5 |:

 instance Semigroup (Stream m a) where
  x <> y =
   Stream $ \inNil inCons ->
    unStream x (unStream y inNil inCons) $ \xv xs ->
     inCons xv (xs <> y)

 instance Monoid (Stream m a) where
  mempty = sNil

 instance Functor (Stream m) where
  fmap f x =
   Stream $ \inNil inCons ->
    unStream x inNil $ \xv xs ->
     inCons (f xv) (fmap f xs)

 instance Applicative (Stream m) where
  pure x = sCons x sNil

  f <*> x =
   Stream $ \inNil inCons ->
    unStream f inNil $ \fv fs ->
     unStream (fmap fv x <> (fs <*> x)) inNil inCons

 instance Monad (Stream m) where
  x >>= f =
   Stream $ \inNil inCons ->
    unStream x inNil $ \xv xs ->
     unStream (f xv <> (xs >>= f)) inNil inCons
