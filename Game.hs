#!/usr/bin/env stack
-- stack --resolver lts-11.3 runghc

module Game where
 import Prelude
 import System.IO (stdin, stdout, BufferMode(..), hSetBuffering)
 import Data.Word (Word64)
 import Data.Bits (shift, xor)

 main :: IO ()
 main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  putStrLn ": First comes rock..."
  putStrLn ": Say \"rock\", \"scissors\", or \"paper\"."
  ans <- getLine
  case ans of
   "rock" -> game 1
   _ -> putStrLn ": You are a baby."

 game :: Word64 -> IO ()
 game rd = do
  putStrLn ": Say \"rock\", \"scissors\", or \"paper\"."
  ans <- getLine
  case rd `mod` 4 of
   0 -> do
    putStrLn ": rock"
    case ans of
     "rock" -> game (xorshift64 rd)
     "scissors" -> putStrLn ": I win."
     "paper" -> putStrLn ": You win."
     _ -> putStrLn ": You are a baby."
   1 -> do
    putStrLn ": scissors"
    case ans of
     "rock" -> putStrLn ": You win."
     "scissors" -> game (xorshift64 rd)
     "paper" -> putStrLn ": I win."
     _ -> putStrLn ": You are a baby."
   2 -> do
    putStrLn ": paper"
    case ans of
     "rock" -> putStrLn ": I win."
     "scissors" -> putStrLn ": You win."
     "paper" -> game (xorshift64 rd)
     _ -> putStrLn ": You are a baby."
   _ -> game (xorshift64 rd)

 xorshift64 :: Word64 -> Word64
 xorshift64 = lShift 17 . lShift (-7) . lShift 13

 lShift :: Int -> Word64 -> Word64
 lShift x y = y `xor` shift y x
