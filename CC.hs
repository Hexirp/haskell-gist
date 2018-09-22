module CC where
 import Prelude

 import Criterion.Main

 import C

 main :: IO ()
 main = do
  x <- defaultMain [
   bgroup "cham" [
    bench "10000" $ nf cham 10000,
    bench "100000" $ nf cham 100000,
    bench "1000000" $ nf cham 1000000]
   ]
  return x

 cham :: Int -> [Bool]
 cham x = take x champernowne
