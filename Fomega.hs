{-# LANGUAGE LambdaCase #-}

-- | Fomegaは集合を基礎とするプログラミング言語である。
module Fomega where
  data Fomega a 
    = Value a -- ^ A
    | Application (Fomega a) (Fomega a) -- ^ F A
    | Function a (Fomega a) (Fomega a)  -- ^ (A:T)->B
    | Singleton (Fomega a) -- ^ !A
    | Union (Fomega a) (Fomega a) -- ^ A|B

  instance (Eq a) => Eq (Fomega a) where
    a == b = reduce a `eq` reduce b
      where
        eq
          :: (Eq a)
          => Fomega a -- ^ 簡約済み
          -> Fomega a -- ^ 簡約済み
          -> Bool
        eq (Value a)         (Value b)         = a == b
        eq (Application f a) (Application g b) = f `eq` g && a`eq` b
        eq (Function a t b)  (Function c s d)  = a == c   && t `eq` s && b `eq` d
        eq (Singleton a)     (Singleton b)     = a `eq` b
        eq (Union a b)       (Union c d)       = a `eq` c && b `eq` d

  instance (Show a) => Show (Fomega a) where
    show (Value a)         = show a
    show (Application a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Function a t b)  = "(" ++ show a ++ ":" ++ show t ++ ")" ++ "->" ++ show b
    show (Singleton a)     = "!" ++ show a
    show (Union a b)       = show a ++ "|" ++ show b

  run
    :: (Eq a)
    => Fomega a -- ^ 未簡約
    -> Maybe a
  run = take_out . reduce

  take_out
    :: Fomega a -- ^ 簡約済み
    -> Maybe a
  take_out = \case
    Value a -> Just a
    _       -> Nothing

  reduce
    :: (Eq a)
    => Fomega a -- ^ 未簡約
    -> Fomega a -- ^ 簡約済み
  reduce = \case
    Value a         -> Value a
    Application a b -> apply (reduce a) (reduce b)
    Function a t b  -> Function a (reduce t) (reduce b)
    Singleton a     -> Singleton (reduce a)
    Union a b       -> Union (reduce a) (reduce b)

  apply
    :: (Eq a)
    => Fomega a -- ^ 簡約済み
    -> Fomega a -- ^ 簡約済み
    -> Fomega a -- ^ 簡約済み
  apply f@(Function fa ft fb) a
    | a `belongs_to` ft          = replace fa a fb
    | otherwise                  = Application f a
  apply f                     a  = Application f a

  belongs_to
    :: (Eq a)
    => Fomega a　-- ^ 簡約済み
    -> Fomega a -- ^ 簡約済み
    -> Bool
  belongs_to a (Singleton b) = a == b
  belongs_to a (Union t s)   = a `belongs_to` t && a `belongs_to` s
  belongs_to _  _            = False

  replace
    :: (Eq a)
    => a        -- ^ 置き換え元
    -> Fomega a -- ^ 置き換え先
    -> Fomega a -- ^ 対象
    -> Fomega a
  replace a b = let my = replace a b in \case
    v@(Value x)
      | a == x          -> b
      | otherwise       -> v
    v@(Application f x) -> Application (my f) (my x)
    v@(Function x t y)
      | a == x          -> Function x (my t) y
      | otherwise       -> Function x (my t) (my y)
    v@(Singleton x)     -> Singleton (my x)
    v@(Union x y)       -> Union (my x) (my y)

  main :: IO ()
  main = do
    let universe = (foldr1 Union . map Value) [0..9]
    let y = Function 0 universe (Application (Function 1 universe (Application (Value 0) (Application (Value 1) (Value 1)))) (Function 1 universe (Application (Value 0) (Application (Value 1) (Value 1)))))
    print $ y
    print $ Application y (Value 2)