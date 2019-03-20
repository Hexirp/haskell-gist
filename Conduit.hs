{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

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

{-

runVessel :: Vessel i o u r -> Str i u -> Str o r
runVessel (Done r)    _ = Nil r
runVessel (Yield o k) s = Cons o (runVessel k s)
runVessel (Await e f) s = case s of
 Nil u    -> undefined (f u)
 Cons i s -> runVessel (e i) s

runVessel :: Vessel i o u r -> Str i u -> Str o (Vessel i o u r)
runVessel (Done r)    _ = undefined
runVessel (Yield o k) s = Cons o (runVessel k s)
runVessel (Await e f) s = case s of
 Nil u    -> Nil (f u)
 Cons i s -> runVessel (e i) s

-}

-- runVessel a b は a の実行結果に従い b から要素を取り出していく。
runVessel :: Vessel i o u r -> Str i u -> Str o (Either (Vessel i o u r) (Str i u, r))
runVessel (Done r)    s = Nil (Right (s, r))
runVessel (Yield o k) s = Cons o (runVessel k s)
runVessel (Await e f) s = case s of
 Nil u    -> Nil (Left (f u))
 Cons i s -> runVessel (e i) s

-- compose a b は a の実行結果に従い b から要素を取り出していく。
compose :: Vessel c d e f -> Vessel a b c d -> Vessel a b e f
compose (Done r)    t = undefined
compose (Yield o k) t = Yield o (compose k t)
compose (Await e f) t = case t of
 Done u -> undefined
 Yield c t -> compose (e c) t
 Await a b -> Await (\x -> compose (Await e f) (a x)) (\x -> compose (Await e f) (b x))


main :: IO ()
main = return ()
