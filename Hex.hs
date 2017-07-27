module Language.Hex where
 import Prelude (Char, String)

 -- | Combinetors
 --
 -- > void <> a === void
 --
 -- > void >> a === a
 -- > a >> void === a
 --
 -- > a <> a === a
 -- > a <> (b <> c) === (a <> b) <> c
 --
 -- > a >> (b >> c) === (a >> b) >> c
 --
 -- > (a1 <> a2) >> b === (a1 >> b) <> (a2 >> b)
 -- > a >> (b1 <> b2) === (a >> b1) <> (a >> b2)
 class Combinable p where
  void :: p

  (<>) :: p -> p -> p

  (>>) :: p -> p -> p

  some :: p -> p
  some p = p <> void

  many :: p -> p
  many p = (p >> many p) <> void

  more :: p -> p
  more p = (p >> more p) <> p

 class Combinable p => Parsingable p where
  char :: Char -> p

  parse :: p -> String -> Bool
