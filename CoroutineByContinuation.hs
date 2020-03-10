-- stack --resolver=lts-12.26 runghc --package=transformers
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

  import Prelude

  import Control.Monad ( join )

  import Control.Monad.IO.Class

  import Data.IORef

  import Control.Monad.Trans.Cont
  import Control.Monad.Trans.Reader

  -- タイトル通り。
  --
  -- https://medium.com/@star_zero/callback%E5%BD%A2%E5%BC%8F%E3%81%AE%E3%82%82%E3%81%AE%E3%82%92coroutines%E3%81%AB%E5%AF%BE%E5%BF%9C%E3%81%99%E3%82%8B-9384dfa6ad77
  --
  -- coroutine by continuation でググったらこんなのがあった。
  --
  -- newtype Callback = Callback { runCallback :: Either SomeException String -> IO () }
  --
  -- execute :: Callback -> IO ()
  --
  -- suspend :: (Continuation a -> IO ()) -> IO T
  --
  -- execute に suspend を適用して IO String にするみたいな話になる。
  --
  -- 思っていたのと違う。
  
  -- newtype Coroutine s m r = Coroutine { resume :: m (Either (s (Coroutine s m r)) r) }

  -- data Yield a b c = Yield a (b -> c)

  -- https://www.stackage.org/haddock/lts-15.3/freer-simple-1.2.1.1/Control-Monad-Freer-Coroutine.html
  --
  -- https://www.stackage.org/haddock/lts-15.3/extensible-effects-5.0.0.1/Control-Eff-Coroutine.html
  --
  -- https://www.stackage.org/haddock/lts-15.3/monad-coroutine-0.9.0.4/Control-Monad-Coroutine.html
  --
  -- わーい、いっぱいあるなあ。
  --
  -- Free (Yield a b) === Coroutine (Yield a b) Identity
  --
  -- おそらく、こうなる。
  
  -- data Step a r = More a r | Done

  -- newtype Source m a = Source { unSource :: m (Step a (Source m a)) }

  -- https://github.com/fumieval/coroutine/blob/a7c7081aeab53f4887897b1d7259f10aacc8609e/Coroutine.hs
  --
  -- これを参考にした。
  --
  -- Source IO になっているのを Source (Cont IO) にすればいいんかな？
  --
  -- data Step a r = More a r として、
  --
  -- Coroutine (Step a) m () === Source m a となる。たぶん。
  --
  -- ここで、前者は実行にステップがあるわけだけど、後者にはない。
  
  -- emptySource :: Applicative m => Source m a
  -- emptySource = Source $ pure Done

  -- type SourceBuilder r a = IORef (Source (ContT r IO) a)

  -- yield :: forall r a. a -> ReaderT (SourceBuilder r a) (ContT (Step a (Source (ContT r IO) a)) IO) ()
  -- yield x = ReaderT $ \r -> do
  --   result <- shiftT $ \cont -> pure $ More x $ Source $ resetT $ liftIO $ cont Done
  --   liftIO $ writeIORef r $ Source $ pure result
  -- {-# INLINE yield #-}

  -- runCoroutine :: ReaderT (SourceBuilder r a) (ContT (Step a (Source (ContT r IO) a)) IO) () -> Source (ContT r IO) a
  -- runCoroutine m = Source $ resetT $ do
  --   ref <- liftIO $ newIORef emptySource
  --   runReaderT m ref
  --   result <- liftIO $ readIORef ref
  --   undefined unSource result

  main :: IO ()
  main = return ()

  -- うーん。型が合わない。
  
  -- newtype Cor s m a = Cor { runCor :: forall r. (Either (s (Cor s m a)) a -> m r) -> m r }

  -- newtype Cor s m a = Cor { runCor :: forall r. (s (Cor s m a) -> m r) -> (a -> m r) -> m r }

  newtype Cio r o a = Cio { runCio :: (a -> IO o) -> IO r }

  evalCio :: Cio r o o -> IO r
  evalCio m = runCio m pure

  fmapCio :: (a -> b) -> Cio r o a -> Cio r o b
  fmapCio f m = Cio (\k -> runCio m (\x -> k (f x)))

  pureCio :: a -> Cio r r a
  pureCio x = Cio (\k -> k x)

  bindCio :: Cio r i a -> (a -> Cio i o b) -> Cio r o b
  bindCio m f = Cio (\k -> runCio m (\x -> runCio (f x) k))

  joinCio :: Cio r i (Cio i o a) -> Cio r o a
  joinCio m = Cio (\k -> runCio m (\n -> runCio n k))

  shiftCio :: ((a -> Cio i i o) -> Cio r j j) -> Cio r o a
  shiftCio f = Cio (\k0 -> runCio (f (\x -> Cio (\k1 -> join (fmap k1 (k0 x))))) pure)

  resetCio :: Cio a o o -> Cio r r a
  resetCio m = Cio (\k -> join (fmap k (runCio m pure)))

  liftIOCio :: IO a -> Cio r r a
  liftIOCio m = Cio $ \k -> join (fmap k m)

  data Step a r = More a r | Done

  newtype Source m a = Source { unSource :: m (Step a (Source m a)) }

  yield :: a -> IORef (Source (Cio _ _) a) -> Cio _ _ ()
  yield x ref = (shiftCio $ \k -> pure $ More x $ Source $ resetCio $ k Done) `bindCio` (\res -> liftIOCio $ writeIORef ref $ Source $ pureCio res)
