{-# LANGUAGE MagicHash #-}

module Unsafe where

 import Prelude

 import Unsafe.Coerce

 import GHC.Exts
 import GHC.Show

 -- >>> main
 -- 2305843009213693952
 --
 -- これは二進法で
 --
 -- 10000000000000000000000000000000000000000000000000000000000000
 --
 -- 内部表現が露出している感が強い。
 main :: IO ()
 main = print $ (unsafeCoerce True :: Int#)

 showIntHash :: Int# -> String
 showIntHash n# = itos n# []
