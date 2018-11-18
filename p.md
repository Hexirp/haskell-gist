https://twitter.com/kmb0k0/status/1063741318977867776?s=21

わかった……！

論理学者は、1番目の条件で8パターンを見つける。2番目の条件で総和を知り、
パターンを絞るが、それで解けない。これによって、私たちは、論理学者が絞った
パターンは総和が等しいと分かる。だから、私たちは論理学者が絞ったパターンは
(1, 4, 9), (1, 6, 6) だと知る。3番目の条件で、一番上の子が一人っ子であることを
知り、解く。私たちも同様に解く。

A. 1, 4, 9

```text
$ stack ghci
Using main module: 1. Package `hexirp-blog' component exe:hexirp-blog-exe with main-is file: (---)\blog\app\Main.hs
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: hexirp-blog
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Lib              ( (---)\blog\src\Lib.hs, interpreted )
[2 of 2] Compiling Main             ( (---)\blog\app\Main.hs, interpreted )
Ok, two modules loaded.
Loaded GHCi configuration from (---)\haskell-stack-ghci\88e753de\ghci-script
*Main Lib> print [ (m, n, o) | m <- [1..36], n <- [1..36], o <- [1..36], 36 == m * n * o ]
[(1,1,36),(1,2,18),(1,3,12),(1,4,9),(1,6,6),(1,9,4),(1,12,3),(1,18,2),(1,36,1),(2,1,18),(2,2,9),(2,3,6),(2,6,3),(2,9,2),(2,18,1),(3,1,12),(3,2,6),(3,3,4),(3,4,3),(3,6,2),(3,12,1),(4,1,9),(4,3,3),(4,9,1),(6,1,6),(6,2,3),(6,3,2),(6,6,1),(9,1,4),(9,2,2),(9,4,1),(12,1,3),(12,3,1),(18,1,2),(18,2,1),(36,1,1)]
*Main Lib> let x = [ (m, n, o) | m <- [1..36], n <- [1..36], o <- [1..36], 36 == m * n * o ]
*Main Lib> print $ map (\(m, n, o) -> m + n + o) x
[38,21,16,14,13,14,16,21,38,21,13,11,11,13,21,16,11,10,10,11,16,14,10,14,13,11,11,13,14,13,14,16,16,21,21,38]
*Main Lib> let y = filter (\(m, n, o) -> (m < n) && (m < o)) x
*Main Lib> print x
[(1,1,36),(1,2,18),(1,3,12),(1,4,9),(1,6,6),(1,9,4),(1,12,3),(1,18,2),(1,36,1),(2,1,18),(2,2,9),(2,3,6),(2,6,3),(2,9,2),(2,18,1),(3,1,12),(3,2,6),(3,3,4),(3,4,3),(3,6,2),(3,12,1),(4,1,9),(4,3,3),(4,9,1),(6,1,6),(6,2,3),(6,3,2),(6,6,1),(9,1,4),(9,2,2),(9,4,1),(12,1,3),(12,3,1),(18,1,2),(18,2,1),(36,1,1)]
*Main Lib> let y = filter (\(m, n, o) -> (    n) && (m < o)) x

<interactive>:6:52: error: lexical error at character '\ESC'
*Main Lib> let y = filter (\(m, n, o) => (n < m) && (o < m)) x

<interactive>:7:28: error: parse error on input ▒e=>▒f
*Main Lib> let y = filter (\(m, n, o) -> (n < m) && (o < m)) x
*Main Lib> print y
[(4,3,3),(6,2,3),(6,3,2),(9,1,4),(9,2,2),(9,4,1),(12,1,3),(12,3,1),(18,1,2),(18,2,1),(36,1,1)]
*Main Lib> print $ map (\(m, n, o) -> m + n + o) y
[10,11,11,14,13,14,16,16,21,21,38]
*Main Lib> let z = filter (\(m, n. o) -> (o <= n)) y

<interactive>:11:22: error: Parse error in pattern: n . o
*Main Lib> let z = filter (\(m, n, o) -> o <= n) y
*Main Lib> print z
[(4,3,3),(6,3,2),(9,2,2),(9,4,1),(12,3,1),(18,2,1),(36,1,1)]
*Main Lib> print $ map (\(m, n, o) -> m + n + o) z
[10,11,13,14,16,21,38]
*Main Lib> let x = [ (m, n, o) | m <- [1..36], n <- [1..36], o <- [1..36], 36 == m * n * o ]
*Main Lib> let y = filter (\(m, n, o) -> (m <= n) && (n <= o)) x
*Main Lib> print y
[(1,1,36),(1,2,18),(1,3,12),(1,4,9),(1,6,6),(2,2,9),(2,3,6),(3,3,4)]
*Main Lib> let z = filter (\(m, n, o) -> n < o) y
*Main Lib> print z
[(1,1,36),(1,2,18),(1,3,12),(1,4,9),(2,2,9),(2,3,6),(3,3,4)]
*Main Lib> print $ map (\(m, n, o) -> m + n + o) z
[38,21,16,14,13,11,10]
*Main Lib> print $ map (\(m, n, o) -> m + n + o) y
[38,21,16,14,13,13,11,10]
*Main Lib> print y
[(1,1,36),(1,2,18),(1,3,12),(1,4,9),(1,6,6),(2,2,9),(2,3,6),(3,3,4)]
*Main Lib> print $ map (\(m, n, o) -> m + n + o) z
[38,21,16,14,13,11,10]
*Main Lib> print z
[(1,1,36),(1,2,18),(1,3,12),(1,4,9),(2,2,9),(2,3,6),(3,3,4)]
*Main Lib> :quit
Leaving GHCi.
```
