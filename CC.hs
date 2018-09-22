module CC where
 import Prelude

 import Criterion.Main

 import C

 main :: IO ()
 main = do
  x <- defaultMain [
   bgroup "cham" [
    bench "10000" $ nf champernowne 10000,
    bench "100000" $ nf champernowne 100000,
    bench "1000000" $ nf champernowne 1000000]
   ]
  return x
