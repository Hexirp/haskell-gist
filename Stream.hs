module Stream where
 import Prelude
 
 import Control.Monad (join)

 newtype Streamly m a =
  Streamly {
   unStreamly :: forall r. m r -> (a -> Streamly m a -> m r) -> m r
  }

 sNil :: Streamly m a
 sNil = Streamly $ \inNil _ -> inNil

 sCons :: a -> Streamly m a -> Streamly m a
 sCons x xs = Streamly $ \_ inCons -> inCons x xs

 sConsM :: Monad m => m a -> Streamly m a -> Sreamly m a
 sConsM mx xs = Streamly $ \_ inCons -> join $ inCons <$> mx <*> pure xs
