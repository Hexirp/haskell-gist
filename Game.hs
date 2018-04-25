#!/usr/bin/env stack
-- stack --resolver lts-11.3 runghc

module Game where
 import Prelude
 import Data.IORef (IORef, newIORef, readIORef, writeIORef)
 import System.IO (stdin, stdout, BufferMode(..), hSetBuffering)
 import Control.Exception (evaluate)
 import Data.Word (Word64)
 import Data.Bits (shift, xor)

 -- Application

 main :: IO ()
 main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  rd <- newIORef 3
  game0 rd

 game0 :: IORef Word64 -> IO ()
 game0 rd = do
  putStrLn ": First comes rock..."
  putStrLn ": Say \"rock\", \"scissors\", or \"paper\"."
  ans <- getHand
  case ans of
   Just Rock -> do
    putStrLn ": rock"
    game1 rd
   _ -> putStrLn ": You are a baby."

 game1 :: IORef Word64 -> IO ()
 game1 rd = do
  putStrLn ""
  putStrLn ": Say \"rock\", \"scissors\", or \"paper\"."
  ans <- getHand
  case ans of
   Just theirs -> do
    ours <- randomHand rd
    putStrLn $ ": " ++ showHand ours
    battle
        (putStrLn ": I win.")
        (putStrLn ": You win.")
        (game1 rd)
        ours
        theirs
   Nothing -> putStrLn ": You are a baby."

 randomHand :: IORef Word64 -> IO Hand
 randomHand ref = do
  rd  <- readIORef ref
  rd' <- evaluate (xorshift64 rd)
  writeIORef ref rd'
  case rd' `mod` 4 of
   0 -> return Rock
   1 -> return Scissors
   2 -> return Paper
   3 -> randomHand ref

 -- Logic

 data Hand = Rock | Scissors | Paper

 readHand :: String -> Maybe Hand
 readHand "rock"     = Just Rock
 readHand "scissors" = Just Scissors
 readHand "paper"    = Just Paper
 readHand _          = Nothing

 getHand :: IO (Maybe Hand)
 getHand = readHand <$> getLine
 
 showHand :: Hand -> String
 showHand Rock     = "rock"
 showHand Scissors = "scissors"
 showHand Paper    = "paper"

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

 -- Xorshift

 xorshift64 :: Word64 -> Word64
 xorshift64 =  xShift 17 . xShift (-7) . xShift 13

 xShift :: Int -> Word64 -> Word64
 xShift x y = y `xor` shift y x
