{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- iteratee, conduit, ... などのストリーム処理ライブラリは [String] -> [Int] と
-- いうようなリストからリストへの関数をよりよく表すものである。このような関数は
-- 多くの場合、遅延評価によって省スペースで処理できる。しかし、この性質は簡単に
-- 壊れてしまう。それを簡単には壊れないように修正したものが、ストリーミングライ
-- ブラリであるのだ。

map' :: (a -> b) -> [a] -> [b]
map' f = go where
 go x = case x of       --   await
  []     -> []          --   done
  x : xs -> f x : go xs --   yield & continue

-- 上のコードで図示されるように conduit における基本的な動作が既に map に含まれ
-- ている。ちなみに、これはパターンマッチングによる評価の強制により駆動するので
-- pull 式ということになるのだと思う。

data StrF a b i = NilF b | ConsF a i

data Str a b = Nil b | Cons a (Str a b)

-- このようなデータ構造を考える。ここでリストを使わないのは終端処理で値を返した
-- い場合があるからだ。つまりリストでは return を実現できない。

data Vessel i o u r
 = Done r
 | Yield o (Vessel i o u r)
 | Await (i -> Vessel i o u r) (u -> Vessel i o u r)

-- runVessel a b は a の実行結果に従い b から要素を取り出していく。
runVessel :: Vessel i o u r -> Str i u -> Str o r
runVessel = goR where
 goR :: Vessel i o u r -> Str i u -> Str o r
 goR s t = case s of
  Done sr -> Nil sr -- t を切り捨てる。
  Yield so sk -> Cons so (goR sk t)
  Await se sf -> goL se sf t
 goL :: (i -> Vessel i o u r) -> (u -> Vessel i o u r) -> Str i u -> Str o r
 goL se sf t = case t of
  Nil tr -> goR (sf tr) (Nil tr) -- 打ち止めや。はよ終われ。
  Cons to tk -> goR (se to) tk

-- fuse a b は a の実行結果に従い b から要素を取り出していく。
fuse :: forall a b c d e f. Vessel b e d f -> Vessel a b c d -> Vessel a e c f
fuse = goR where
 goR :: Vessel b e d f -> Vessel a b c d -> Vessel a e c f
 goR s t = case s of
  Done sr -> Done sr
  Yield so sk -> Yield so (goR sk t)
  Await se sf -> goL se sf t
 goL :: (b -> Vessel b e d f) -> (d -> Vessel b e d f) -> Vessel a b c d -> Vessel a e c f
 goL se sf t = case t of
  Done tr -> goR (sf tr) (Done tr)
  Yield to tk -> goR (se to) tk
  Await te tf -> Await (\a -> goL se sf (te a)) (\c -> goL se sf (tf c))

listToStr :: [a] -> Str a ()
listToStr [] = Nil ()
listToStr (x : xs) = Cons x (listToStr xs)

listToVessel :: [a] -> Vessel x0 a x1 ()
listToVessel [] = Done ()
listToVessel (x : xs) = Yield x (listToVessel xs)

strToVessel :: Str a b -> Vessel x0 a x1 b
strToVessel (Nil r) = Done r
strToVessel (Cons x xs) = Yield x (strToVessel xs)

main :: IO ()
main = return ()
