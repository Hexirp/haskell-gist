module Main where

  import Prelude

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
  
  newtype Coroutine s m r = Coroutine { resume :: m (Either (s (Coroutine s m r)) r) }

  data Yield a b c = Yield a (b -> c)

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
