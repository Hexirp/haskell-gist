#!/usr/bin/env stack
-- stack --resolver lts-11.3 runghc

module Game where
  import Prelude
  import System.IO (stdin, stdout, BufferMode(..), hSetBuffering)
  import Data.Word (Word64)
  import Data.Bits (shift)

  main :: IO ()
  main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    putStrLn ": Answer one natural number."
    ans <- readLn :: IO Integer
    putStrLn $ game ": You answered " "." ": You are a baby" ans

    test

  game :: String -> String -> String -> Integer -> String
  game s0 s1 s2 x
    | 0 <= x    = s0 ++ show x ++ s1
    | otherwise = s2

  test :: IO ()
  test = do
    print $ (maxBound                 :: Word64)
    print $ ((2 ^ 64 - 1)             :: Word64)
    print $ ((2 ^ 64 - 1) `div` 2     :: Word64)
    print $ (shift (2 ^ 64 - 1) (-1)  :: Word64)
    print $ (lShift (-1) (2 ^ 64 - 1) :: Word64)
    print $ ((2 ^ 64 - 3)             :: Word64)
    print $ ((2 ^ 64 - 3) `div` 2     :: Word64)
    print $ (shift (2 ^ 64 - 3) (-1)  :: Word64)
    print $ (lShift (-1) (2 ^ 64 - 3) :: Word64)
    print $ ((2 ^ 64 - 3)             :: Word64)
    print $ ((2 ^ 64 - 3) `div` 4     :: Word64)
    print $ (shift (2 ^ 64 - 3) (-2)  :: Word64)
    print $ (lShift (-2) (2 ^ 64 - 3) :: Word64)

  xorshift64 :: Word64 -> Word64
  xorshift64 = lShift 17 . lShift (-7) . lShift 13

  lShift :: Int -> Word64 -> Word64
  lShift x y
    | 0 <= x    = shift y x
    | otherwise = y `div` 2 ^ (-x)
