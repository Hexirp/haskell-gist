#!/usr/bin/env stack
-- stack --resolver lts-11.3 runghc

module Game where
  import Prelude

  main :: IO ()
  main = do
    putStrLn ": Answer one natural number."
    ans <- readLn :: IO Integer
    case 0 <= ans of
      False -> putStrLn ": You are a baby."
      True -> putStrLn $ ": You answered " ++ show ans ++ "."
