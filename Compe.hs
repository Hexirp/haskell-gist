{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Compe where
 import Prelude

 import Control.Category ((>>>))

 import Data.Kind (Type)

 import Control.Monad.Skeleton (Skeleton, debone, MonadView(Return, (:>>=)))

 import Control.Exception (throwIO)

 import Text.Read (readMaybe)

 data Comparg :: Type -> Type where
  Get :: Read a => Comparg a
  Vec :: (Read a, Integral i) => i -> Comparg [a]
  Str :: Comparg String

  Do :: IO a -> Comparg a

 run :: [String] -> Comparg a -> IO a
 run s = debone >>> \case
  Return a -> return a
  Get :>>= f -> pop s $ \x xs -> parse x $ \v -> run xs $ f v
  Vec i :>>= f -> iter s i $ \vs s' -> run s' (f vs)
  Str :>>= f -> pop s $ \x xs -> run xs (f x)
  Do e :>>= f -> e >>= \x -> run (f x)

 pop :: [String] -> (String -> [String] -> IO a) -> IO a
 pop [] _ = throwIO $ userError "Argument is missing!"
 pop (x : xs) f = f x xs

 parse :: Read a => String -> (a -> IO b) -> IO b
 parse s f = case readMaybe s of
  Nothing -> throwIO $ userError "Parse Error!"
  Just a -> f a

 iter :: Read a => [String] -> Integer -> ([a] -> [String] -> IO b) -> IO b
 iter s 0 f = f [] s
 iter s n f = pop s $ \x xs -> parse x $ \v -> iter xs (n - 1) $ \vs s' -> f (v : vs) s')

 run' :: String -> Comparg a -> IO a
 run' s = run (words s)
