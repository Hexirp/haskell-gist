module Sort where
  import Prelude hiding ()

  infixl 0 -:
  (-:) = flip ($)

  -- 以下の法則を常に満たしていなければならない。
  -- > prop_sorted_list :: (Ord a) => SortedList a -> Bool
  -- > prop_sorted_list Unit = True -- law_1
  -- > prop_sorted_list (UnsafeCons x Unit) = True -- law_2
  -- > prop_sorted_list (UnsafeCons x y@(UnsafeCons yv _)) = x <= yv && prop_sorted_list y -- law_3
  -- つまり、[1,2,3,3,4,8]というようなリストであるという事である。
  data SortedList a
    = Unit
    | UnsafeCons a (SortedList a)

  single :: a -> SortedList a
  single a = UnsafeCons a Unit -- ここのUnsafeConsはlaw_2より安全である。

  cons :: (Ord a) => (Either (SortedList a) (a, a, SortedList a) -> b) -> a -> SortedList a -> b
  cons f x Unit = f $ Left $ single x
  cons f x y@(UnsafeCons yv ys) = if x <= yv -- assum
   then f $ Left $ UnsafeCons x y -- ここのUnsafeConsはassumとlaw_3より安全である。
   else f $ Right $ (x, yv, ys)

  infixr 5 $:
  ($:) :: (Ord a) => a -> SortedList a -> Maybe (SortedList a)
  ($:) = cons $ either Just $ const Nothing

  insertion_sort :: (Ord a) => [a] -> SortedList a
  insertion_sort [] = Unit
  insertion_sort (x:xs) = insert_ x $ insertion_sort xs
    where
      insert_ = cons $ either id $ \(x, yv, ys) -> UnsafeCons yv $ insert_ x ys -- ここのUnsafeConsは!assumとlaw_3よｒ安全である。