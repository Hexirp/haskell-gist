module StreamTest where
 import Prelude

 import Stream

 i1 :: Iteratee () IO Int
 i1 = do
  iYield' 1
  iYield' 2

 i2 :: Iteratee Int IO ()
 i2 = do
  a <- iAwait'
  iYieldM' $ print a
  b <- iAwait'
  iYieldM' $ print b
 
 i12 :: Iteratee () IO ()
 i12 = iCompose i1 i2
