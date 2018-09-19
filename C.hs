-- チャンパーノウンの定数（底２） / Champernowne constant (base 2)
--
-- 良質な疑似乱数のために
--
-- メリット:
--
-- * 繰り返しがない
-- * 全てのパターンが必ず現れる
--
-- デメリット:
--
-- * 値を取り出し続けると計算量が増える（おそらく log n に比例して）
module C where
 import Prelude

 import Control.Monad (join)

 -- [1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, ...]
 -- [1] ++ [1, 0, 1, 1] ++ [1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1] ++ ...
 -- concatMap (1 :) [[]] ++ concatMap (1 :) [[0], [1]] ++ concatMap (1 :) [[0, 0], [0, 1], [1, 0], [1, 1]] ++ ...

 champernowne :: [Bool]
 champernowne = concat cham_sep

 -- champernowne + separated
 cham_sep :: [[Bool]]
 cham_sep = map cham_piece [0 :: Integer ..]

 -- champernowne + piece
 cham_piece :: Integer -> [Bool]
 cham_piece = map (1 :) . pow

 pow :: Integer -> [Bool]
 pow = concat . pows

 pows :: Integer -> [[Bool]]
 pows 0 = []
 pows n = join $ (:) <$> [0, 1] <*> pows (n - 1)
