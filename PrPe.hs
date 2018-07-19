module PrPe where
 import Prelude

 escape :: String -> String
 escape [] = []
 escape ('\\' : xs) = '\\' : '\\' : escape xs
 escape (x    : xs) = x : escape xs

 lines1 :: String -> [String]
 lines1 = go id where
  go :: (String -> String) -> String -> [String]
  go f []          = f [] : []
  go f ('\n' : xs) = f [] : go id xs
  go f (x    : xs) = go (f . (x :)) xs
 
 lines2 :: String -> [String]
 lines2 []          = [] : []
 lines2 ('\n' : xs) = [] : lines2 xs
 lines2 (x    : xs) = lines2_f x (lines2 xs)

 lines2_f :: Char -> [String] -> [String]
  -- It's impossible case, but I have good answer.
 lines2_f x []       = (x : []) : []
 lines2_f x (y : ys) = (x : y) : ys
