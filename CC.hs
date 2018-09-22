module CC where
 import Prelude

 import Criterion.Main

 import C

 main :: IO ()
 main = do
  x <- defaultMain [
   bgroup "cham" [
    bench "100000" $ nf cham 100000,
    bench "200000" $ nf cham 200000,
    bench "300000" $ nf cham 300000,
    bench "400000" $ nf cham 400000,
    bench "500000" $ nf cham 500000,
    bench "600000" $ nf cham 600000,
    bench "700000" $ nf cham 700000,
    bench "800000" $ nf cham 800000,
    bench "900000" $ nf cham 900000,
    bench "1000000" $ nf cham 1000000,
    bench "1100000" $ nf cham 1100000,
    bench "1200000" $ nf cham 1200000,
    bench "1300000" $ nf cham 1300000,
    bench "1400000" $ nf cham 1400000,
    bench "1500000" $ nf cham 1500000,
    bench "1600000" $ nf cham 1600000,
    bench "1700000" $ nf cham 1700000,
    bench "1800000" $ nf cham 1800000,
    bench "1900000" $ nf cham 1900000]
   ]
  return x

 cham :: Int -> [Bool]
 cham x = take x champernowne
