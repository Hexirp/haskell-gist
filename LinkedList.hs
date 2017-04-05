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
  -- | LinkedListは双方向連結リストを表す。
  data LinkedList a = LNil () () (LinkedList a) -- ^ 左の終端を表す。
                    | Node (LinkedList a) a (LinkedList a) -- ^ 中間ノードを表す。
                    | RNil (LinkedList a) () () -- ^ 右の終端を表す。

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
  traverseLeft (LNil () () _) = []
  traverseLeft (Node a x _) = x : traverseLeft a
  traverseLeft (RNil a () _) = traverseLeft a

  traverseRight :: LinkedList a -> [a]
  traverseRight (LNil _ () a) = traverseRight a
  traverseRight (Node _ x a) = x : traverseRight a
  traverseRight (RNil _ () ()) = []

  instance (Show a) => Show (Tape a) where
    show (TLNil () () a) = "." ++ show a
    show (TNode a x b) = show a ++ "[" ++ show x ++ "]" ++ show b
    show (TRNil a () ()) = show a ++ "."

  instance (Show a) => Show (LinkedList a) where
    show = show . toTapeFromLinked

  moveLeft :: LinkedList a -> Maybe (LinkedList a)
  moveLeft (LNil () () a) = Nothing
  moveLeft (Node a x b) = Just a
  moveLeft (RNil a () ()) = Just a

  moveRight :: LinkedList a -> Maybe (LinkedList a)
  moveRight (LNil () () a) = Just a
  moveRight (Node a x b) = Just b
  moveRight (RNil a () ()) = Nothing

  type Trampoline a b = a -> Either b a

  toTrampoline :: (a -> Maybe a) -> Trampoline a a
  toTrampoline f x = case f x of
    Nothing -> Left x
    Just a -> Right a

  runTrampoline :: Trampoline a b -> a -> b
  runTrampoline f x = case f x of
    Left b -> b
    Right a -> runTrampoline f a

  runLoop :: (a -> Maybe a) -> a -> a
  runLoop = runTrampoline . toTrampoline

  moveToLeftEnd :: LinkedList a -> LinkedList a
  moveToLeftEnd = runLoop moveLeft

  moveToRightEnd :: LinkedList a -> LinkedList a
  moveToRightEnd = runLoop moveRight