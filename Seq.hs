{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module : Seq
-- Description : Sequent calculus by Arrow
-- Copyright : (c) Hexirp, 2017
-- License : BSD3
-- 
-- Arrowは計算を表現します。それはシークエント計算と類似しています。
-- 一例として、cut規則は合成に対応し、init規則は恒等射に対応します。
-- さらにこの関係を広げれば、命題論理や、線形論理などを表せると予想
-- しました。
module Seq where
 import Prelude ()

 -- | 線形論理の性質を持つ。
 class Linear seq where
  -- infixl 5 :@
  data (:@) :: * -> * -> *
  -- infixl 9 :*
  data (:*) :: * -> * -> *
  -- infixl 8 :&
  data (:&) :: * -> * -> *
  -- infixl 7 :+
  data (:+) :: * -> * -> *
  -- infixl 6 :|
  data (:|) :: * -> * -> *

  data Wow :: * -> *
  data Hmm :: * -> *

  data One :: *
  data Top :: *
  data Zer :: *
  data Bot :: *

  init :: seq a a

  cut :: seq gg (dd :@ x) -> seq (x :@ ss) pp -> seq (gg :@ ss) (dd :@ pp)
