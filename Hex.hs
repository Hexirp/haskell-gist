{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Language.Hex where
 import Prelude

 data List :: * -> * where
  Nil :: List a
  Cons :: a -> List a -> List a
 
 data InList :: k -> List k -> * where
  InListBase :: InList k (Cons k a)
  InListRec :: InList a l -> InList a (Cons k l)
 
 data Field' :: * where
  Field :: String -> a -> Field
 
 data MkField :: Field -> *
  MkField :: a -> MkField (Field s a)
