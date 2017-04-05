module LinkedList
  ( LinkedList(..)
  , empty
  , one
  , toLinked
  , Tape(..)
  , toTape
  , toLinkedFromTape
  , toTapeFromLinked
  , traverseLeft
  , traverseRight
  ) where
  data LinkedList a = LNil () () (LinkedList a)
                    | Node (LinkedList a) a (LinkedList a)
                    | RNil (LinkedList a) () ()

  empty :: LinkedList a
  empty = l
    where
      l = LNil () () r
      r = RNil l () ()

  one :: a -> LinkedList a
  one x = l
    where
      l = LNil () () n
      n = Node l x r
      r = RNil n () ()

  toLinked :: [a] -> LinkedList a
  toLinked = toLinkedFromTape . toTape

  data Tape a = TLNil () () [a]
              | TNode [a] a [a]
              | TRNil [a] () ()

  toTape :: [a] -> Tape a
  toTape = TLNil () ()

  toLinkedFromTape :: Tape a -> LinkedList a
  toLinkedFromTape (TLNil () () a) = case a of
    [] -> LNil () () (toLinkedFromTape $ TRNil [] () ())
    (x:xs) -> LNil () () (toLinkedFromTape $ TNode [] x xs)
  toLinkedFromTape (TNode a b c) = case (a, c) of
    ([], []) -> Node (toLinkedFromTape $ TLNil () () [b]) b (toLinkedFromTape $ TRNil [b] () ())
    (x:xs, []) -> Node (toLinkedFromTape $ TNode xs x [b]) b (toLinkedFromTape $ TRNil (b:x:xs) () ())
    ([], x:xs) -> Node (toLinkedFromTape $ TLNil () () (b:x:xs)) b (toLinkedFromTape $ TNode [b] x xs)
    (x:xs, y:ys) -> Node (toLinkedFromTape $ TNode xs x (b:y:ys)) b (toLinkedFromTape $ TNode (b:x:xs) y ys)
  toLinkedFromTape (TRNil a () ()) = case a of
    [] -> RNil (toLinkedFromTape $ TLNil () () []) () ()
    (x:xs) -> RNil (toLinkedFromTape $ TNode xs x []) () ()

  toTapeFromLinked :: LinkedList a -> Tape a
  toTapeFromLinked (LNil () () a) = TLNil () () (traverseRight a)
  toTapeFromLinked (Node a x b) = TNode (traverseLeft a) x (traverseRight b)
  toTapeFromLinked (RNil a () ()) = TRNil (traverseLeft a) () ()

  traverseLeft :: LinkedList a -> [a]
  traverseLeft = undefined

  traverseRight :: LinkedList a -> [a]
  traverseRight = undefined