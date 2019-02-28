module PrPe where
 import Prelude

 escape :: String -> String
 escape [] = []
 escape ('\\' : xs) = '\\' : '\\' : escape xs
 escape (x    : xs) = x : escape xs

 -- lines1 === lines2 === lines4 === lines5 === lines6 === lines7 =\= lines3
 --
 -- lines4 and lines5 causes a space leak. linesN is expected to have O(n)
 -- performance ... n is the length of the string. But, lines4 and lines5 have
 -- O(n^2) performance.

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
 lines3 (x    : xs) = lines3_f x (lines3 xs)

 lines3_f :: Char -> [String] -> [String]
 lines3_f x []       = (x : []) : []
 lines3_f x (y : ys) = (x : y) : ys

 lines4 :: String -> [String]
 lines4 = go [] where
  go :: String -> String -> [String]
  go acc []          = acc : []
  go acc ('\n' : xs) = acc : go [] xs
  go acc (x    : xs) = go (acc ++ [x]) xs

 lines5 :: String -> [String]
 lines5 = go [] where
  go :: String -> String -> [String]
  go acc []          = reverse acc : []
  go acc ('\n' : xs) = reverse acc : go [] xs
  go acc (x    : xs) = go (x : acc) xs

 lines6 :: String -> [String]
 lines6 = uncurry (:) . go where
  go :: String -> (String, [String])
  go []          = ([], [])
   -- Alternatively, ([], lines6 xs)
  go ('\n' : xs) = let (y, ys) = go xs in ([], y : ys)
  go (x    : xs) = let (y, ys) = go xs in (x : y, ys)

 lines7 :: String -> [String]
 lines7 = go (:) where
  go :: (String -> [String] -> r) -> String -> r
  go k []          = k [] []
  go k ('\n' : xs) = go (\y ys -> k [] (y : ys)) xs
  go k (x    : xs) = go (\y ys -> k (x : y) ys) xs

 -- Strict test
 test1 :: (String -> [String]) -> Int -> IO ()
 test1 f n = let src = src1 n in print src >> print (f src)

 src1 :: Int -> String
 src1 n = concat $ replicate n (loop n ('a' :) "\n")

 loop :: Int -> (a -> a) -> a -> a
 loop 0 _ x = x
 loop n f x = f (loop (n - 1) f x)
