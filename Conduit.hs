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
compose :: forall a b c d e f. Vessel b e d f -> Vessel a b c d -> Vessel a e c f
compose s t = case s of
 Done sr -> undefined sr t
 Yield so sk -> Yield so (compose sk t)
 Await se sf -> case t of
  Done tr -> undefined (sf tr)
  Yield to tk -> compose (se to) tk
  Await te tf -> Await (\a -> compose s (te a)) (\c -> compose s (tf c))

data Vessela a b c d e f = Cut f (Vessel a b c d) | Out (Vessel b e d f)

-- compose の書き直し。
fuse :: forall a b c d e f. Vessel b e d f -> Vessel a b c d -> Vessel a e c (Vessela a b c d e f)
fuse s t = goR s t where
 goR :: Vessel b e d f -> Vessel a b c d -> Vessel a e c f
 goR s t = case s of
  Done sr -> Done (_ sr t)
  Yield so sk -> Yield so (goR sk t)
  Await se sf -> goL se sf t
 goL :: (b -> Vessel b e d f) -> (d -> Vessel b e d f) -> Vessel a b c d -> Vessel a e c (Vessela a b c d e f)
 goL se sf t = case t of
  Done tr -> Done (_ (sf tr))
  Yield to tk -> goR (se to) tk
  Await te tf -> Await (\a -> goL se sf (te a)) (\c -> goL se sf (tf c))

main :: IO ()
main = return ()
