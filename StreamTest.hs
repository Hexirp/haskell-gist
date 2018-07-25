module StreamTest where
 import Prelude

 import Stream

 s1 :: Stream IO ()
 s1 = print 1 |: print 2 |: sNil

 s2 :: Stream IO ()
 s2 = print 3 |: print 4 |: sNil

 s12 :: Stream IO ()
 s12 = s1 <> s2

 i1 :: Iteratee () IO Int
 i1 = iYield 1 $ iYield 2 iDone

 i2 :: Iteratee Int IO ()
 i2 = iAwait $ \a -> iYieldM (print a) $ iAwait $ \b -> iYieldM (print b) iDone
 
 i12 :: Iteratee () IO ()
 i12 = iCompose i1 i2

 c1 :: Conduit () Int () IO ()
 c1 = do
  cYield' 1
  cYield' 2

 c2 :: Conduit Int () () IO ()
 c2 = do
  a <- cAwait'
  cYieldM' $ print a
  b <- cAwait'
  cYieldM' $ print b

 c12 :: Conduit () () () IO ()
 c12 = cCompose c1 c2
