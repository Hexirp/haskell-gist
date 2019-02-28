module RST where

 line :: String -> [String]
 line = go id where
  go :: (String -> String) -> String -> [String]
  go f []          = f [] : []
  go f ('\n' : xs) = f [] : go id xs
  go f (x    : xs) = go (f . (x :)) xs
 
 paragraph :: [String] -> [[String]]
 paragraph = go id where
  go :: ([String] -> [String]) -> [String] -> [[String]]
  go f []          = f [] : []
  go f ("\n" : xs) = f [] : go id xs
  go f (x    : xs) = go (f . (x :)) xs
