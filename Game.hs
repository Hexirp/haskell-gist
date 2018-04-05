#!/usr/bin/env stack
-- stack --resolver lts-11.3 runghc

module Game where
  import Prelude
  import System.IO (stdin, stdout, BufferMode(..), hGetBuffering) 

  main :: IO ()
  main = do
    si_bf <- hGetBuffering stdin
    print si_bf
    so_bf <- hGetBuffering stdout
    print so_bf
    putStrLn ": Answer one natural number."
    ans <- readLn :: IO Integer
    case 0 <= ans of
      False -> putStrLn ": You are a baby."
      True -> putStrLn $ ": You answered " ++ show ans ++ "."
