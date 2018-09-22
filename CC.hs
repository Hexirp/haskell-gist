module CC where
 import Prelude

 import Criterion.Main

 import C

 main :: IO ()
 main = do
  x <- defaultMain [
   bgroup "cham" [
    bench "10000" $ nf cham 10000,
    bench "20000" $ nf cham 20000,
    bench "30000" $ nf cham 30000,
    bench "40000" $ nf cham 40000,
    bench "50000" $ nf cham 50000,
    bench "60000" $ nf cham 60000,
    bench "70000" $ nf cham 70000,
    bench "80000" $ nf cham 80000,
    bench "90000" $ nf cham 90000,
    bench "100000" $ nf cham 100000,
    bench "110000" $ nf cham 110000,
    bench "120000" $ nf cham 120000,
    bench "130000" $ nf cham 130000,
    bench "140000" $ nf cham 140000,
    bench "150000" $ nf cham 150000,
    bench "160000" $ nf cham 160000,
    bench "170000" $ nf cham 170000,
    bench "180000" $ nf cham 180000,
    bench "190000" $ nf cham 190000]
   ]
  return x

 cham :: Int -> [Bool]
 cham x = take x champernowne
