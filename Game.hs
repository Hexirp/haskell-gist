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
    case 0 <= ans of
      False -> putStrLn ": You are a baby."
      True -> putStrLn $ ": You answered " ++ show ans ++ "."

    test

  shiftWord64 :: Int -> Word64 -> Int
  shiftWord64 x y = fromEnum $ shift y x

  test :: IO ()
  test = do
    print $ (shiftWord64 (-1) maxBound) == (maxBound `div` 2)
