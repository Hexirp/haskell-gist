module PrPe where
 import Prelude

 escape :: String -> String
 escape [] = []
 escape ('\\' : xs) = '\\' : '\\' : escape xs
 escape (x    : xs) = x : escape xs

 -- lines1 === lines2
 --
 -- lines1 "" = [""]
 -- lines2 "" = [""]
 -- lines3 "" = []
 --
 -- lines1 "\n" = ["",""]
 -- lines2 "\n" = ["",""]
 -- lines3 "\n" = [""]
 --
 -- lines1 "\n\n" = ["","",""]
 -- lines2 "\n\n" = ["","",""]
 -- lines3 "\n\n" = ["",""]

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
 
 lines3 :: String -> [String]
 lines3 []          = []
 lines3 ('\n' : xs) = [] : lines3 xs
 lines3 (x    : xs) = lines3_f x (lines2 xs)

 lines3_f :: Char -> [String] -> [String]
 lines3_f x []       = (x : []) : []
 lines3_f x (y : ys) = (x : y) : ys

 lines4 :: String -> [String]
 lines4 = go [] where
  go :: String -> [String] -> [String]
  go acc []          = acc : []
  go acc ('\n' : xs) = acc : xs
  go acc (x    : xs) = go (acc ++ (x : [])) xs
