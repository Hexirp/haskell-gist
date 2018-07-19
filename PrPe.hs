module PrPe where
 import Prelude

 -- | Escape \\.
 escape :: String -> String
 escape [] = []
 escape ('\\' : xs) = '\\' : '\\' : escape xs
 escape (x    : xs) = x : escape xs
