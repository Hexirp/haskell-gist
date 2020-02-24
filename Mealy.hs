module Mealy where

  import Prelude

  newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

  instance Functor (Mealy i) where
    fmap f = go_0 where
      go_0 (Mealy x) = Mealy (\a -> go_1 (x a))
      go_1 (b, x) = (f b, go_0 x)

  instance Applicative (Mealy i) where
    pure b = go where
      go = Mealy (\a -> (b, go))
    Mealy m <*> Mealy n = Mealy (\a -> case m a of (f, m') -> case n a of (b, n') -> (f b, m' <*> n'))

  instance Monad (Mealy i) where
    Mealy m >>= f = Mealy (\a -> case m a of (b0, m') -> case f b0 of Mealy n -> case n a of (b1, n') -> (b1, m' >>= undefined f n'))

  -- data Reader r :: Type -> Type where
  --   Ask :: forall r. Reader r r
  --
  -- Coyoneda (Reader r) a
  --   ~
  -- sigma b, (b -> a, Reader r b)
  --   ~
  -- r -> a
  --
  -- Coyoneda (Reader r) は、そのままモナドになることができる。
  --
  -- Free (Coyoneda (Reader r)) もモナドになることができる。
  
  newtype Mealy a b = Mealy { runMealy :: forall r. a -> (b -> Mealy a b -> r) -> r }

  instance Functor (Mealy i) where
    fmap f = go where
      go (Mealy x) = Mealy (\a k -> x a (\b x' -> k (f b) (go x')))

  instance Applicative (Mealy i) where
    pure b = go where
      go = Mealy (\a k -> k b go)
    Mealy m <*> Mealy n = Mealy (\a k -> m a (\f m' -> n a (\b n' -> k (f b) (m' <*> n'))))
