{-# LANGUAGE LambdaCase #-}

module LFSR where
  import Prelude hiding ((!!), init)

  lfsr_18 :: [Bool]
  lfsr_18 = init 18 ++ lfsr lfsr_18_M lfsr_18

  init :: Integer -> [Bool]
  init = \case
    0 -> []
    n -> True : init (n - 1)

  lfsr_18_M :: [[a] -> a]
  lfsr_18_M = (!! 0):(!! 7):[]

  (!!) :: [a] -> Integer -> a
  (!!) = \case
    []      -> undefined
    (a : s) -> \case
      0 -> a
      n -> s !! (n - 1)

  lfsr :: [[Bool] -> Bool] -> [Bool] -> [Bool]
  lfsr f s@(_ : x) = (foldr xor False $ ($ s) <$> f) : lfsr f x

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

  cost :: Int -> IO ()
  cost = \n -> do
    putStrLn $ fmap toBinary $ take n $ lfsr_18