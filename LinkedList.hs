module LinkedList
  ( LinkedList(..)
  , empty
  , one
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
