module Lifegame where

 import Prelude
 import Control.Concurrent (threadDelay)

 main :: IO ()
 main = loop new

 loop :: Array -> IO ()
 loop a = do
  threadDelay 10000000
  putStrLn $ view a
  loop (update a)


 type Array = Int -> Int -> Bool

 --  #
 --   #
 -- ###
 new :: Array
 new = \x_ y_ -> let
   x = x_ `mod` 20
   y = y_ `mod` 20
  in
   False
    || (x == 0 && y == 1)
    || (x == 1 && y == 2)
    || (x == 2 && y == 0)
    || (x == 2 && y == 1)
    || (x == 2 && y == 2)

 update :: Array -> Array
 update a = \x_ y_ -> let
  x = x_ `mod` 20
  y = y_ `mod` 20
  b = a x y
  n = 0
   + bti (a (x - 1) (y - 1))
   + bti (a (x - 1) (y + 0))
   + bti (a (x - 1) (y + 1))
   + bti (a (x + 0) (y - 1))
   -- + bti (a (x + 0) (y + 0))
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
 bti True  = 1

 view :: Array -> String
 view a = let
   a' = fmap (<$> [ 0 .. 19 ]) $ (<$> [ 0 .. 19 ]) $ a
  in
   bits a'

 bits :: [[Bool]] -> String
 bits []       = ""
 bits (x : xs) = bit x $ ('\n' :) $ bits xs

 bit :: [Bool] -> String -> String
 bit []           = id
 bit (False : xs) = ('-' :) . bit xs
 bit (True  : xs) = ('#' :) . bit xs
