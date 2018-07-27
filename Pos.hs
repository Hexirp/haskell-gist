#!/usr/bin/env stack
-- stack --resolver lts-12.0 --install-ghc runghc --package random

module Main where
 import Prelude
 import System.Random

 rx :: (Int, Int)
 rx = (1000, 1999)

 ry :: (Int, Int)
 ry = (0, 999)

 rP :: IO (Int, Int)
 rP = (,) <$> randomRIO r <*> randomRIO r

 rPs :: [IO (Int, Int)]
 rPs = replicate 10 rP

 fom :: (Int, Int) -> String
 fom (a, b) = "| | " ++ show a ++ " | " ++ show b ++ " |"

 foms :: [(Int, Int)] -> String
 foms = unlines . map fom

 frPs :: IO String
 frPs = foms <$> sequence rPs

 main :: IO ()
 main = frPs >>= putStrLn
