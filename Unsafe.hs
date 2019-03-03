{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples #-}

module Unsafe where

 import Prelude

 import Unsafe.Coerce

 import GHC.Exts
 import GHC.Show

 -- >>> main
 -- 2305843009213693952
 --
 -- これは二進法で
 --
 -- 10000000000000000000000000000000000000000000000000000000000000
 --
 -- 内部表現が露出している感が強い。
 main :: IO ()
 main = putStrLn $ showUnboxedInt (unsafeCoerce True :: Int#)

 showUnboxedInt :: Int# -> String
 showUnboxedInt n# = itos n# []

 itos :: Int# -> String -> String
 itos n# cs
     | isTrue# (n# <# 0#) =
         let !(I# minInt#) = minInt in
         if isTrue# (n# ==# minInt#)
                 -- negateInt# minInt overflows, so we can't do that:
            then '-' : (case n# `quotRemInt#` 10# of
                        (# q, r #) ->
                            itos' (negateInt# q) (itos' (negateInt# r) cs))
            else '-' : itos' (negateInt# n#) cs
     | otherwise = itos' n# cs
     where
     itos' :: Int# -> String -> String
     itos' x# cs'
         | isTrue# (x# <# 10#) = C# (chr# (ord# '0'# +# x#)) : cs'
         | otherwise = case x# `quotRemInt#` 10# of
                       (# q, r #) ->
                           case chr# (ord# '0'# +# r) of
                           c# ->
                               itos' q (C# c# : cs')
