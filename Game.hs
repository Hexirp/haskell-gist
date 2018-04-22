#!/usr/bin/env stack
-- stack --resolver lts-11.3 runghc

module Game where
 import Prelude
 import System.IO (stdin, stdout, BufferMode(..), hSetBuffering)
 import Data.Word (Word64)
 import Data.Bits (shift, xor)

 -- Application

 main :: IO ()
 main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  game0 1

 game0 :: Word64 -> IO ()
 game0 rd = do
  putStrLn ": First comes rock..."
  putStrLn ": Say \"rock\", \"scissors\", or \"paper\"."
  ans <- getHand
  case ans of
   Just Rock -> game1 1
   _ -> putStrLn ": You are a baby."

 game1 :: Word64 -> IO ()
 game1 rd = do
  putStrLn ": Say \"rock\", \"scissors\", or \"paper\"."
  let (ours, rd') = randomHand rd
  ans <- getHand
  case ans of
   Just theirs -> do
    case ours of
     Rock     -> putStrLn ": rock"
     Scissors -> putStrLn ": scissors"
     Paper    -> putStrLn ": paper"
    battle (putStrLn ": I win.") (putStrLn ": You win.") (game1 rd') ours theirs
   Nothing -> putStrLn ": You are a baby."

 xorshift64 :: Word64 -> Word64
 xorshift64 = xShift 17 . xShift (-7) . xShift 13

 xShift :: Int -> Word64 -> Word64
 xShift x y = y `xor` shift y x

 -- Logic

 data Hand = Rock | Scissors | Paper

 readHand :: String -> Maybe Hand
 readHand "rock"     = Just Rock
 readHand "scissors" = Just Scissors
 readHand "paper"    = Just Paper
 readHand _          = Nothing

 getHand :: IO (Maybe Hand)
 getHand = readHand <$> getLine

 randomHand :: Word64 -> (Hand, Word64)
 randomHand rd =
  let
   rd' = xorshift64 rd
  in
   case rd `mod` 4 of
    0 -> (Rock,     rd')
    1 -> (Scissors, rd')
    2 -> (Paper,    rd')
    3 -> randomHand rd'

 battle :: a -> a -> a -> Hand -> Hand -> a
 battle gt lt eq x y =
  case x of
   Rock ->     case y of
    Rock ->     eq
    Scissors -> gt
    Paper ->    lt
   Scissors -> case y of
    Rock ->     lt
    Scissors -> eq
    Paper ->    gt
   Paper ->    case y of
    Rock ->     gt
    Scissors -> lt
    Paper ->    eq
