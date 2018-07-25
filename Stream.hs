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

 sCons' :: a -> Stream m a
 sCons' x = sCons x sNil

 sConsM' :: Monad m => m a -> Stream m a
 sConsM' mx = sConsM mx sNil

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

 sRun :: Stream IO () -> IO ()
 sRun x = unStream x (return ()) (\_ xs -> sRun xs)

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

 iMapSrc :: (s -> t) -> Iteratee t m a -> Iteratee s m a
 iMapSrc f x =
  Iteratee $ \done yield await ->
   unIteratee x
    done
    (\xv xs -> yield xv (iMapSrc f xs))
    (\xw -> await (\s -> iMapSrc f (xw (f s))))

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
   unIteratee y
    done
    (\yv ys -> yield yv (iCompose x ys))
    (\yw -> unIteratee (iCompose' x yw) done yield await)

 iCompose' :: Iteratee a m b -> (b -> Iteratee b m c) -> Iteratee a m c
 iCompose' x yw =
  Iteratee $ \done yield await ->
   unIteratee x
    done
    (\xv xs -> unIteratee (iCompose xs (yw xv)) done yield await)
    (\xw -> await (\s -> iCompose' (xw s) yw))

 iRun :: Iteratee () IO () -> IO ()
 iRun x = unIteratee x (return ()) (\_ xs -> iRun xs) (\xw -> iRun (xw ()))

 newtype Conduit i o u m a =
  Conduit {
   unConduit :: forall r.
    (a -> m r) ->
    (o -> Conduit i o u m a -> m r) ->
    ((i -> Conduit i o u m a) -> (u -> Conduit i o u m a) -> m r) ->
    m r
  }

 cDone :: a -> Conduit i o u m a
 cDone xv = Conduit $ \done _ _ -> done xv

 cYield :: o -> Conduit i o u m a -> Conduit i o u m a
 cYield xo xs = Conduit $ \_ yield _ -> yield xo xp

 cAwait :: (i -> Conduit i o u m a) -> (u -> Conduit i o u m a) -> Conduit i o u m a
 cAwait xp xc = Conduit $ \_ _ await -> await xp xc

 cYieldM :: Monad m => m o -> Conduit i o u m a -> Conduit i o u m a
 cYieldM mxo xs = Conduit $ \_ yield _ -> join $ yield <$> mxo <*> pure xs

 cYield' :: o -> Conduit i o u m ()
 cYield' xo = cYield xo (cDone ())

 cAwait' :: (i -> Conduit i o () m ()) -> Conduit i o () m ()
 cAwait' xp = cAwait xp (\_ -> cDone ())

 cYieldM' :: Monad m => m o -> Conduit i o u m ()
 cYieldM' mxo = cYieldM mxo (cDone ())

 instance Functor (Conduit i o u m) where
  fmap f x =
   Conduit $ \done yield await ->
    unConduit x
     (\xv -> done (f xv))
     (\xo xs -> yield xo (fmap f xs))
     (\xp xc -> await (\i -> fmap f (xp i)) (\u -> fmap f (xc u)))

 instance Applicative (Conduit i o u m) where
  pure x = cDone x

  f <*> x =
   Conduit $ \done yield await ->
    unConduit f
     (\fv -> unConduit (fmap fv x) done yield await)
     (\fo fs -> yield fo (fs <*> x))
     (\fp fc -> await (\i -> fp i <*> x) (\u -> fc u <*> x))
 
 instance Monad (Conduit i o u m) where
  x >>= f =
   Conduit $ \done yield await ->
    unConduit x
     (\xv -> unConduit (f xv) done yield await)
     (\xo xs -> yield xo (xs >>= f))
     (\xp xc -> await (\i -> xp i >>= f) (\i -> xc i >>= f))
 
 cCompose :: Conduit a c b m d -> Conduit c e d m f -> Conduit a e b m f
 cCompose x y =
  Conduit $ \done yield await ->
   unConduit y
    (\yv -> done yv)
    (\yo ys -> yield yo (cCompose x ys))
    (\yp yc -> unConduit (cCompose' x yp yc) done yield await)

 cCompose' :: Conduit a c b m d -> (c -> Conduit c e d m f) -> (d -> Conduit c e d m f) -> Conduit a e b m f
 cCompose' x yp yc =
  Conduit $ \done yield await ->
   unConduit x
    (\xv -> unConduit (cCompose (cDone xv) (yc xv)) done yield await)
    (\xo xs -> unConduit (cCompose xs (yp xo)) done yield await)
    (\xp xc -> await (\i -> cCompose' (xp i) yp yc) (\u -> cCompose' (xc u) yp yc))
 
 cRun :: Conduit () () () IO () -> IO ()
 cRun x = unConduit x (\xv -> return xv) (\_ xs -> cRun xs) (\xp _ -> cRun (xp ()))
