import Unsafe.Coerce

{-# NOINLINE bool_var1 #-}
bool_var1 :: Bool
bool_var1 = True

main :: IO ()
main = print $ (unsafeCoerce bool_var1 :: Int)
