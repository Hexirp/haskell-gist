module StreamTest where
 import Prelude

 import Stream

 s1 :: Stream IO ()
 s1 = print 1 |: print 2 |: iNil

 s2 :: Stream IO ()
 s2 = print 3 |: print 4 |: iNil

 s12 :: Stream IO ()
 s12 = s1 <> s2

 i1 :: Iteratee () IO Int
 i1 = iYield 1 $ iYield 2 $ iYield 3 iDone

 i2 :: Iteratee Int IO ()
 i2 = iAwait $ \a -> iYieldM (print a) $ iAwait $ \b -> iYieldM (print b) iDone
 
 i12 :: Iteratee () IO ()
 i12 = iCompose i1 i2
