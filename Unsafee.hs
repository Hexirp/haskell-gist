{-# LANGUAGE MagicHash #-}

module Main where

import Unsafe.Coerce
import GHC.Exts


data CompactD = CA | CB | CC
  deriving (Show)

data LargeD = LA | LB | LC | LD | LE | LF | LG | LH
  deriving (Show)

data IntOrBool
  = IBI# Int#
  | IBB Bool
  deriving (Show)

data MyInt = MyI# Int#
  deriving (Show)


-- |
--
-- macOS: 10.14.3
-- CPU: Intel Core i7
-- Memory: 8GB DDR3
-- GHC: 8.6.3
-- Option: -O1
--
-- >>> main
-- 4315527696
-- MyI# 4315527696#
-- IBI# 4315527696#
-- 5764607523051092410
-- MyI# 5764607523051092410#
-- CB
-- 10
-- CB
-- CA
-- CA
--
main :: IO ()
main = do
  print (unsafeCoerce False :: Int)
  print (unsafeCoerce False :: MyInt)
  print (unsafeCoerce False :: IntOrBool)
  print (unsafeCoerce True :: Int)
  print (unsafeCoerce True :: MyInt)
  -- print (unsafeCoerce True :: IntOrBool) -- SIGSEGV
  print (unsafeCoerce True :: CompactD)
  print (unsafeCoerce (IBI# 10#) :: Int)
  print (unsafeCoerce (IBB True) :: CompactD)
  print (unsafeCoerce LA :: CompactD)
  print (unsafeCoerce LD :: CompactD)
