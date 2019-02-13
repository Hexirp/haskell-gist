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

 fmap0 :: NFData a => (b0 -> b1) -> Trampoline0 a b0 -> Trampoline0 a b1
 fmap0 m f = Trampoline0 $ \a -> Right $ m $ run4 f a

 pure0 :: b -> Trampoline0 a b
 pure0 x = Trampoline0 (\_ -> Right x)

 fapp0
  :: NFData a
  => Trampoline a (b0 -> b1)
  -> Trampoline0 a b0
  -> Trampoline0 a b1
 fapp0 m f = Trampoline0 $ \a -> run4 m a $ run4 f a

 bind0
  :: NFData a
  => Trampoline0 a b0
  -> (b0 -> Trampoline0 a b1)
  -> Trampoline0 a b1
 bind0 f m = Trampoline0 $ \a -> m $ run4 f a

 prod0
  :: (NFData a0, NFData a1)
  => Trampoline0 a0 b0
  -> Trampoline0 a1 b1
  -> Trampoline0 (a0, a1) (b0, b1)
 prod0 f g
  = Trampoline0 $ \(a0, a1) -> (run4 f a0, run4 g a1)

 data Trampoline1 :: * -> * -> * where
  Trampoline1 :: a -> (a -> Either a b) -> Trampoline1 a b

 run5 :: NFData a => Trampoline1 a b -> b
 run5 (Trampoline1 z f) = go z where
  go x = x `deepseq` case f x of
   Left a -> go a
   Right b -> b

 data Trampoline2 :: * -> * where
  Trampoline2 :: NFData a => a -> (a -> Eithr a b) -> Trampoline2 b

 run6 :: Trampoline2 b -> b
 run6 (Trampoline2 z f) = go z where
  go x = x `deepseq` case f x of
   Left a -> go a
   Right b -> b
