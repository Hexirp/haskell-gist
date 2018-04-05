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
    putStrLn $ game ans

  game :: Integer -> String
  game x
    | 0 <= x    = ": You answered " ++ show x ++ "."
    | otherwise = ": You are a baby."

  xorshift64 :: Word64 -> Word64
  xorshift64 = lShift 17 . lShift (-7) . lShift 13

  lShift :: Int -> Word64 -> Word64
  lShift x y
    | 0 <= x    = shift y x
    | otherwise = div y (2 ^ x)
