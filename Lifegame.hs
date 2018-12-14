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

 update :: Array -> Array
 update a = \x y -> let
  b = a x y
  n = 0
   + bti (a (x - 1) (y - 1))
   + bti (a (x - 1) (y + 0))
   + bti (a (x - 1) (y + 1))
   + bti (a (x + 0) (y - 1))
   + bti (a (x + 0) (y + 0))
   + bti (a (x + 0) (y + 1))
   + bti (a (x + 1) (y - 1))
   + bti (a (x + 1) (y + 0))
   + bti (a (x + 1) (y + 1))
  in
   next b n

 next :: Bool -> Int -> Bool
 next False 3 = True
 next False _ = False
 next True  2 = True
 next True  3 = True
 next True  _ = False

 bti :: Bool -> Int
 bti False = 0
 bti True = 1
