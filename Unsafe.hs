module Unsafe where

 import Prelude

 import Unsafe.Coerce

 -- >>> main
 -- 2305843009213693952
 --
 -- これは二進法で
 --
 -- 10000000000000000000000000000000000000000000000000000000000000
 --
 -- 内部表現が露出している感が強い。
 main :: IO ()
 main = putStrLn $ show (unsafeCoerce True :: Int)
