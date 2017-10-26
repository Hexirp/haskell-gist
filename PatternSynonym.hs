#!/usr/bin/env stack
-- stack --resolver lts-9.10 --install-ghc ghci

{-# LANGUAGE PatternSynonyms #-}

data SExpr = Cons SExpr SExpr | AtomInt Int

newtype CallowSExpr = CallowSExpr { growup :: SExpr }

pattern AtomInt' :: Int -> CallowSExpr
pattern AtomInt' x = CallowSExpr (AtomInt x)

pattern Cons' :: SExpr -> SExpr -> CallowSExpr
pattern Cons' x y = CallowSExpr (Cons x y)

main :: IO ()
main = do
  let x = Cons' (AtomInt 1) (AtomInt 2)
  return ()

pattern Cons'' :: CallowSExpr -> CallowSExpr -> CallowSExpr
pattern Cons'' (CallowSExpr x) (CallowSExpr y) = Cons' x y

y :: CallowSExpr
y = Cons'' (AtomInt' 1) (AtomInt' 2)
