module Unsafe where

 import Prelude

 main :: IO ()
 main = print $ (unsafeCoerce True :: Int)
