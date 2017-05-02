-- | A Simple List Module.
--
-- I try to quickly calculate 'flat'.
module List
 ( List(..)
 , NonEmpty(..)
 , Stream(..)
 , flat
 ) where
 -- import Prelude ((.), ($), uncurry, Functor, fmap)

 data List a = Nil | List a (List a)

 data NonEmpty a = Last a | NonEmpty a (NonEmpty a)

 -- | Stream is a infinite list.
 data Stream a = Stream a (Stream a)

 instance Functor Stream where
  fmap f (Stream x xs) = Stream (f x) (fmap f xs)

 -- | Pick up the elements of infinite lists of a infinite list as shown below.
 --
 -- > 0 1 3 6
 -- > 2 4 7
 -- > 5 8
 -- > 9
 flat :: Stream (Stream a) -> Stream a
 flat = repeel . NotMining

 repeel :: Mining (Stream a) -> Stream a
 repeel = uncurry ($) . fmap repeel . mining carve peel

 type Peel a = (a -> a, Mining a)

 carve :: Stream (Stream a) -> Peel (Stream a)
 carve (Stream (Stream x xs) ys) = (Stream x, Mined xs $ NotMining ys)

 peel :: Stream a -> Peel (Stream a) -> Peel (Stream a)
 peel (Stream x xs) (k, s) = (Stream x . k, Mined xs s)

 data Mining a = NotMining (Stream a) | Mined a (Mining a)

 mining :: (Stream a -> b) -> (a -> b -> b) -> Mining a -> b
 mining z _ (NotMining x) = z x
 mining z s (Mined x xs) = s x $ mining z s xs

 -- import Prelude
 
 repeatS :: a -> Stream a
 repeatS a = Stream a $ repeatS a
 
 takeS :: Int -> Stream a -> [a]
 takeS n s = case compare n 0 of
  LT -> error $ "takeS: A Minus Argment..." ++ show n
  EQ -> []
  GT -> case s of
   Stream x xs -> x : takeS (n - 1) xs
 
 streamNum :: Stream Int
 streamNum = Stream 0 $ fmap (1 +) streamNum

 -- Benchmark

 -- 'flat'
 -- > flat (repeatS streamNum)
 -- > (repeel . NotMining) (repeatS streamNum)
 -- > repeel (NotMining (repeatS streamNum))
 -- > (uncurry ($) . fmap repeel . mining carve peel) (NotMining (repeatS streamNum))
 -- > (uncurry ($) . fmap repeel) (mining carve peel (NotMining (repeatS streamNum)))
 -- > uncurry ($) (fmap repeel (mining carve peel (NotMining (repeatS streamNum))))
 -- > uncurry ($) (fmap repeel (carve (repeatS streamNum)))
 -- > uncurry ($) (fmap repeel (carve (Stream streamNum (repeatS streamNum))))
 -- > uncurry ($) (fmap repeel (carve (Stream (Stream 0 $ fmap (1 +) streamNum) (repeatS streamNum))))
 -- > uncurry ($) (fmap repeel (carve (Stream (Stream 0 (fmap (1 +) streamNum)) (repeatS streamNum))))
 -- > uncurry ($) (fmap repeel (Stream 0, Mined (fmap (1 +) streamNum) (NotMining (repeatS streamNum))))
 -- > uncurry ($) (Stream 0, repeel (Mined (fmap (1 +) streamNum) (NotMining (repeatS streamNum))))
 -- > Stream 0 $ repeel (Mined (fmap (1 +) streamNum) (NotMining (repeatS streamNum)))
 
 bench :: Int -> IO ()
 bench n = print $ takeS n $ flat $ repeatS streamNum
