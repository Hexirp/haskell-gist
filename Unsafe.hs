module Unsafe where

 import Prelude

 import Unsafe.Coerce

 main :: IO ()
 main = print $ (unsafeCoerce True :: Int)
