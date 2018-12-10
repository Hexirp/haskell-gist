module Unsafe where

 import Prelude

 import Unsafe.Coerce

 -- >>> main
 -- 2305843009213693952
 main :: IO ()
 main = print $ (unsafeCoerce True :: Int)
