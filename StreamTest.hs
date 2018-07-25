module StreamTest where
 import Prelude

 import Stream

 i1 :: Iteratee () IO Int
 i1 = iYield 1 (iYield 2 (iYield 3 iDone))

 i2 :: Iteratee Int IO ()
 i2 = iAwait (\a -> iYieldM (print a) (iAwait (\b -> iYieldM (print b) iDone)))
 
 i12 :: Iteratee () IO ()
 i12 = iCompose i1 i2
