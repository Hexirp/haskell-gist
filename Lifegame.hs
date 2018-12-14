module Lifegame where

 import Prelude

 main :: IO ()
 main = return ()

 type Array = Int -> Int -> Bool

 --  #
 --   #
 -- ###
 new :: Array
 new = \x_ y_ -> let
   x = x_ `mod` 80
   y = y_ `mod` 80
  in
   False
    || (x == 0 && y == 1)
    || (x == 1 && y == 2)
    || (x == 2 && y == 0)
    || (x == 2 && y == 1)
    || (x == 2 && y == 2)
