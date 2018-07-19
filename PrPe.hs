module PrPe where
 import Prelude

 escape :: String -> String
 escape [] = []
 escape ('\\' : xs) = '\\' : '\\' : escape xs
 escape (x    : xs) = x : escape xs

 lines_ :: String -> [String]
 lines_ = go id where
  go :: (String -> String) -> String -> [String]
  go f []          = f [] : []
  go f ('\n' : xs) = f [] : go id xs
  go f (x    : xs) = go (f . (x :)) xs
