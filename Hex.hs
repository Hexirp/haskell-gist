{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Hex where
 import Prelude

 newtype Peeler t s m a = Peeler { unpeeler :: s -> m (t a, s) }

 instance (Functor t, Functor m) => Functor (Peeler t s m) where
  fmap f (Peeler p) = Peeler $ p'
   where
    p' = \s -> t1 $ p s
    t1 x = fmap t2 x
    t2 (x, y) = (fmap f x, y)
