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

 newtype Iteratee s m a =
  Iteratee {
   unIteratee :: forall r.
    m r ->
    (a -> Iteratee s m a -> m r) ->
    ((s -> Iteratee s m a) -> m r) ->
    m r
  }
 
 iDone :: Iteratee s m a
 iDone = Iteratee $ \done _ _ -> done
 
 iYield :: a -> Iteratee s m a -> Iteratee s m a
 iYield xv xs = Iteratee $ \_ yield _ -> yield xv xs

 iAwait :: (s -> Iteratee s m a) -> Iteratee s m a
 iAwait xw = Iteratee $ \_ _ await -> await xw

 iYieldM :: Monad m => m a -> Iteratee s m a -> Iteratee s m a
 iYieldM mxv xs = Iteratee $ \_ yield _ -> join $ yield <$> mxv <*> pure xs

 (+:) :: Monad m => m a -> Iteratee s m a -> Iteratee s m a
 (+:) = iYieldM

 instance Semigroup (Iteratee s m a) where
  x <> y =
   Iteratee $ \done yield await ->
    unIteratee x
     (unIteratee y done yield await)
     (\xv xs -> yield xv (xs <> y))
     (\xw -> await (\s -> xw s <> y))

 instance Monoid (Iteratee s m a) where
  mempty = iDone

 instance Functor (Iteratee s m) where
  fmap f x = 
   Iteratee $ \done yield await ->
    unIteratee x
     done
     (\xv xs -> yield (f xv) (fmap f xs))
     (\xw -> await (\s -> fmap f (xw s)))
 
 instance Applicative (Iteratee s m) where
  pure x = iYield x iDone

  f <*> x =
   Iteratee $ \done yield await ->
    unIteratee f
     done
     (\fv fs -> unIteratee (fmap fv x <> (fs <*> x)) done yield await)
     (\fw -> await (\s -> fw s <*> x))
 
 instance Monad (Iteratee s m) where
  x >>= f =
   Iteratee $ \done yield await ->
    unIteratee x
     done
     (\xv xs -> unIteratee (f xv <> (xs >>= f)) done yield await)
     (\xw -> await (\s -> xw s >>= f))

 iCompose :: Iteratee a m b -> Iteratee b m c -> Iteratee a m c
 iCompose x y =
  Iteratee $ \done yield await ->
   unIteratee x
    done
    (\xv xs -> unIteratee (iGive xv (iCompose xs y)) done yield await)
    (\xw -> await (\s -> iCompose (xw s) y))

 iGive :: s -> Iteratee s m a -> Iteratee s m a
 iGive s x =
  Iteratee $ \done yield await ->
   unIteratee x
    done
    (\xv xs -> yield xv (iGive s xs))
    (\xw -> unIteratee (xw s) done yield await)
