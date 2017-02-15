{-# LANGUAGE LambdaCase #-}

module LFSR where
  import Prelude hiding (init)

  lfsr_18 :: [Bool]
  lfsr_18 = init 18 ++ lfsr lfsr_18_M lfsr_18

  lfsr_18_M :: [[a] -> a]
  lfsr_18_M = (!! 0):(!! 7):[]

  lfsr :: [[Bool] -> Bool] -> [Bool] -> [Bool]
  lfsr f s@(_ : x) = (foldr xor False $ ($ s) <$> f) : lfsr f x

  init :: Integer -> [Bool]
  init = \case
    0 -> []
    n -> True : init (n - 1)

  xor :: Bool -> Bool -> Bool
  xor = \case
    False -> id
    True -> not

  toBinary :: Bool -> Char
  toBinary = \case
    False -> '0'
    True -> '1'

  main :: IO ()
  main = cost 100

  -- | lfsr_18 の周期は 2^18-1 = 262143 である。実際、 cost (262143 + 18) とすると末尾に18個の1の並びが出現することが観察できる
  cost :: Int -> IO ()
  cost = \n -> do
    putStrLn $ fmap toBinary $ take n $ lfsr_18