{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- iteratee, conduit, ... などのストリーム処理ライブラリは [String] -> [Int] と
-- いうようなリストからリストへの関数をよりよく表すものである。このような関数は
-- 多くの場合、遅延評価によって省スペースで処理できる。しかし、この性質は簡単に
-- 壊れてしまう。それを簡単には壊れないように修正したものが、ストリーミングライ
-- ブラリであるのだ。

map' :: (a -> b) -> [a] -> [b]
map' f = go where
 go x = case x of      --   await
  []     -> []         --   done
  x : xs -> f x : go x --   yield & continue

-- 上のコードで図示されるように conduit における基本的な動作が既に map に含まれ
-- ている。ちなみに、これはパターンマッチングによる評価の強制により駆動するので
-- pull 式ということになるのだと思う。

data Fix f where
 Fix :: forall x. (x -> Fix f) -> f x -> Fix f

data StrF a b i = Nil b | Cons a i

type Str a b = Fix (StrF a b)

-- このようなデータ構造を考える。ここでリストを使わないのは終端処理で値を返した
-- い場合があるからだ。つまりリストでは return を実現できない。

data Vessel' i o u r
 = Yield' o (Vessel' i o u r)
 | Await' (i -> Vessel' i o u r) (u -> Vessel' i o u r)
 | Done' r

-- これが conduit の組み立て方である。これは Free や Coyoneda を型レベルで展開
-- しているので分かりにくい。Str を援用して分かりやすくする。

newtype Freer f a = Freer {
  runFreer :: forall m. Monad m => (forall t. f t -> m t) -> m a }

data VesselF i o u r where
 Await :: VesselF i o u (StrF a b ())
 Yield :: o -> VesselF i o u ()

type Vessel i o u = Freer (VesselF i o u)

runVessel :: Vessel i o u r -> Str i u -> Str o r
runVessel (Freer k) 
