#!/usr/bin/env stack
-- stack --resolver lts-12.20 ghci --package deepseq
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Trampoline where

 import Prelude

 import Control.DeepSeq

 run0 :: NFData a => (a -> Either a b) -> a -> b
 run0 f = go where
  go x = x `deepseq` case f x of
   Left a -> go a
   Right b -> b

 run1 :: NFData a => (forall x. a -> (a -> x) -> (b -> x) -> x) -> a -> b
 run1 f = go where
  go x = x `deepseq` f x go id

 run2 :: NFData a => (a -> (a -> b) -> (b -> b) -> b) -> a -> b
 run2 f = go where
  go x = x `deepseq` f x go id

 run3 :: NFData a => (a -> (a -> b) -> b) -> a -> b
 run3 f = go where
  go x = x `deepseq` f x go

 data Trampoline0 :: * -> * -> * where
  Trampoline0 :: (a -> Either a b) -> Trampoline0 a b

 run4 :: NFData a => Trampoline0 a b -> a -> b
 run4 (Trampoline0 f) = go where
  go x = x `deepseq` case f x of
   Left a -> go a
   Right b -> b
 
 instance Functor (Trampoline0 a) where
  fmap f (Trampoline0 g) = Trampoline0 (fmap (fmap f) g)
 
 instance Applicative (Trampoline0 a) where
  pure x = Trampoline0 (pure (pure x))
  Trampoline0 f <*> Trampoline0 x = Trampoline0 ((<*>) <$> f <*> g)
 
 instance Monad (Trampoline0 a) where
  Trampoline0 x >>= Trampoline0 f = Trampoline0 $ \a -> 

 data Trampoline0 :: * -> * -> * where
  Trampoline0 :: a -> (a -> Either a b) -> Trampoline0 a b

 run4 :: NFData a => Trampoline0 a b -> b
 run4 (Trampoline0 z f) = go z where
  go x = x `deepseq` case f x of
   Left a -> go a
   Right b -> b

 instance Functor (Trampoline0 a) where
  fmap f (Trampoline0 z g) = Trampoline0 z (fmap (fmap f) g)

 -- Applicative (Trampoline0 a) is impossible. I can't make 'pure' because
 -- @a@ may be @Void@.

 data Trampoline1 :: * -> * where
  Trampoline1 :: NFData a => a -> (a -> Eithr a b) -> Trampoline1 b

 run5 :: Trampoline1 b -> b
 run
