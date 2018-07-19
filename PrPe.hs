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
 --
 -- lines1 === lines4, but lines4 is wrong.

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
  go :: String -> String -> [String]
  go acc []          = acc : []
  go acc ('\n' : xs) = acc : go [] xs
  go acc (x    : xs) = go (acc ++ x : []) xs

 lines5 :: String -> [String]
 lines5 = go [] where
  go :: String -> String -> [String]
  go acc []          = reverse acc : []
  go acc ('\n' : xs) = reverse acc : go [] xs
  go acc (x    : xs) = go (x : acc) xs

 -- 正格
 test1 :: (String -> [String]) -> Int -> IO ()
 test1 f n = let src = src1 n in print src >> print (f src)

 src1 :: String
 src1 n = concat $ replicate n (loop n ('a' :) "\n")

 loop :: Int -> (a -> a) -> a -> a
 loop 0 _ x = x
 loop n f x = f (loop (n - 1) f x)
