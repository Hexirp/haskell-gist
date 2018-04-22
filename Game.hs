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
  case rd `mod` 4 of
   0 -> do
    ans <- getHand
    putStrLn ": rock"
    case ans of
     Rock     -> game (xorshift64 rd)
     Scissors -> putStrLn ": I win."
     Paper    -> putStrLn ": You win."
     Other _  -> putStrLn ": You are a baby."
   1 -> do
    ans <- getHand
    putStrLn ": scissors"
    case ans of
     Rock     -> putStrLn ": You win."
     Scissors -> game (xorshift64 rd)
     Paper    -> putStrLn ": I win."
     Other _  -> putStrLn ": You are a baby."
   2 -> do
    ans <- getHand
    putStrLn ": paper"
    case ans of
     Rock     -> putStrLn ": I win."
     Scissors -> putStrLn ": You win."
     Paper    -> game (xorshift64 rd)
     Other _  -> putStrLn ": You are a baby."
   _ -> game (xorshift64 rd)

 xorshift64 :: Word64 -> Word64
 xorshift64 = xShift 17 . xShift (-7) . xShift 13

 xShift :: Int -> Word64 -> Word64
 xShift x y = y `xor` shift y x

 data Hand = Rock | Scissors | Paper | Other String

 readHand :: String -> Hand
 readHand "rock"     = Rock
 readHand "scissors" = Scissors
 readHand "paper"    = Paper
 readHand x          = Other x

 getHand :: IO Hand
 getHand = readHand <$> getLine
