import Unsafe.Coerce

main :: IO ()
main = print $ (unsafeCoerce True :: Int)
