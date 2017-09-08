#!/usr/bin/env stack
{- stack
 --resolver lts-9.2
 --install-ghc
 exec ghci
 --package base
 --package extensible
 --package monad-skeleton
-}
{-# LANGUAGE LambdaCase #-}
module ExtensibleIO where
 import Prelude
 import Control.Arrow ((>>>))
 import Control.Monad.Skeleton
 import Data.Extensible
 import Data.Proxy

 -- | Run a subset of 'IO' by handles.
 runIOSH :: RecordOf (Interpreter IO) xs -> Eff xs a -> IO a
 runIOSH handles = handleEff handles >>> \case
  Return a -> return a
  x :>>= f -> x >>= runIOSH handles . f

 class IOSubset kv where
  runIOS :: proxy kv -> Interpreter IO (AssocValue kv)

 getHandles :: Forall IOSubset xs => RecordOf (Interpreter IO) xs
 getHandles = htabulateFor (Proxy :: Proxy IOSubset) _
