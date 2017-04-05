{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ApplyList where
  data ApplyList :: * -> * where
    Nil :: a -> ApplyList a
    Con :: (x -> a) -> ApplyList x -> ApplyList a

  run :: ApplyList a -> a
  run (Nil a) = a
  run (Con f x) = f (run x)