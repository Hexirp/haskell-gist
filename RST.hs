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
  go f []        = f [] : []
  go f ("" : xs) = f [] : go id xs
  go f (x  : xs) = go (f . (x :)) xs

 title :: a -> ([String] -> a) -> [String] -> a
 title yes no x = case x of
  [t, l]    -> title2 yes no t l
  [l, t, r] -> title3 yes no l t r
  x'        -> no x'

 title2 :: a -> ([String] -> a) -> String -> String -> a
 title2 yes no t l = case t of
  [] -> no [[], l]
  t' -> title2_line yes no t' l

 title2_line :: a -> ([String] -> a) -> String -> String -> a
 title2_line yes no t l = case l of
  '=' : _ -> undefined
  '-' : _ -> undefined
  '`' : _ -> undefined
  ':' : _ -> undefined
  ''' : _ -> undefined
  '"' : _ -> undefined
  '~' : _ -> undefined
  '^' : _ -> undefined
  '_' : _ -> undefined
  '*' : _ -> undefined
  '+' : _ -> undefined
  '#' : _ -> undefined
  '<' : _ -> undefined
  '>' : _ -> undefined

 title3 :: a -> ([String] -> a) -> String -> String -> String -> a
 title3 yes no l t r = undefined
