module CC where
 import Prelude

 import Criterion.Main

 import C

 main :: IO ()
 main = do
  x <- defaultMain [
   bgroup "cham" [
    bench "1000" $ nf cham 1000,
    bench "2000" $ nf cham 2000,
    bench "3000" $ nf cham 3000,
    bench "4000" $ nf cham 4000,
    bench "5000" $ nf cham 5000,
    bench "6000" $ nf cham 6000,
    bench "7000" $ nf cham 7000,
    bench "8000" $ nf cham 8000,
    bench "9000" $ nf cham 9000,
    bench "10000" $ nf cham 10000,
    bench "11000" $ nf cham 11000,
    bench "12000" $ nf cham 12000,
    bench "13000" $ nf cham 13000,
    bench "14000" $ nf cham 14000,
    bench "15000" $ nf cham 15000,
    bench "16000" $ nf cham 16000,
    bench "17000" $ nf cham 17000,
    bench "18000" $ nf cham 18000,
    bench "19000" $ nf cham 19000]
   ]
  return x

 cham :: Int -> [Bool]
 cham x = take x champernowne
