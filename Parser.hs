-- | Parse by Parsing Expression Grammar
module Parser where
  -- | Parser: StateT Maybe String
  newtype Parser a = InParser { outParser :: String -> Maybe (a, String) }

  instance Functor Parser where
    fmap f p = InParser $ \s0 ->
      case outParser p s0 of
        Nothing -> Nothing
        Just (x, s1) -> Just (f x, s1)

  instance Applicative Parser where
    (<*>) = link ($)

    pure x = InParser $ \s ->
      Just (x, s)

  instance Monad Parser where
    p0 >>= f = InParser $ \s0 ->
      case outParser p0 s0 of
        Nothing -> Nothing
        Just (x, s1) -> case outParser (f x) s1 of
          Nothing -> Nothing
          Just (y, s2) -> Just (y, s2)

  -- | Sequence: e1 e2
  link :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  link f p0 p1 = InParser $ \s0 ->
    case outParser p0 s0 of
      Nothing -> Nothing
      Just (x, s1) -> case outParser p1 s1 of
        Nothing -> Nothing
        Just (y, s2) -> Just (f x y, s2)

  -- | Ordered choice: e1 / e2
  choose :: Parser a -> Parser a -> Parser a
  choose p0 p1 = InParser $ \s0 ->
    case outParser p0 s0 of
      Nothing -> outParser p1 s0
      Just (x, s1) -> Just (x, s1)

  -- loop :: Parser () -> Parser ()
  -- loop p0 = InParser $ \s0 ->
  --   case outParser p0 s0 of
  --     Nothing -> Just ((), s0)
  --     Just ((), s1) -> outParser p0 s1

  loop_stream :: Parser a -> Parser [a]
  loop_stream p0 = InParser $ \s0 ->
    case outParser p0 s0 of
      Nothing -> Just ([], s0)
      Just (x, s1) -> outParser ((x :) <$> loop_stream p0) s1

  -- | Zero-or-more: e*
  loop :: (a -> b -> b) -> b -> Parser a -> Parser b
  loop f a p0 = foldr f a <$> loop_stream p0

  -- | One-or-more: e+
  loop_ :: (b -> b -> b) -> Parser b -> Parser b
  loop_ f p0 = InParser $ \s0 ->
    case outParser p0 s0 of
      Nothing -> Nothing
      Just (x, s1) -> outParser (loop f x p0) s1

  -- | Optional: e?
  try :: a -> Parser a -> Parser a
  try a p0 = p0 `choose` pure a

  -- | And-predicate: &e
  future :: Parser a -> Parser ()
  future p = InParser $ \s0 ->
    case outParser p s0 of
      Nothing -> Nothing
      Just (x, s1) -> Just ((), s0)

  -- | Not-predicate: !e
  check :: Parser a -> Parser ()
  check p = InParser $ \s0 ->
    case outParser p s0 of
      Nothing -> Just ((), s0)
      Just (x, s1) -> Nothing

  char :: Char -> Parser Char
  char c0 = InParser $ \s0 ->
    case s0 of
      [] -> Nothing
      (c1 : s1) -> case c0 == c1 of
        False -> Nothing
        True -> Just (c1, s1)

  void :: Parser ()
  void = pure ()

  -- ########

  parse_a_n_b_n :: Parser Int
  parse_a_n_b_n = do
    char 'a'
    n <- try 0 parse_a_n_b_n
    char 'b'
    return $ n + 1

  parse_a_b_a_b :: Parser Int
  parse_a_b_a_b = do
    n0 <- parse_a_n_b_n
    n1 <- loop (+) 0 $ do
      char ' '
      parse_a_n_b_n
    return $ n0 + n1

  print_parse :: (Show a) => Parser a -> String -> IO ()
  print_parse p0 s0 = do
    putStrLn $ "Parsing " ++ show s0 ++ "..."
    case outParser p0 s0 of
      Just (x, "") -> putStrLn $ "Success! The result is " ++ show x ++ "."
      Just (x, s) -> putStrLn $ "Failed! The result is " ++ show x ++ ". The rest is " ++ show s ++ "."
      Nothing -> putStrLn $ "Error!"

  main :: IO ()
  main = do
    let lf = putStrLn ""
    let pu = putStrLn
    let pa = print_parse parse_a_n_b_n
    let pb = print_parse parse_a_b_a_b
    lf
    pu "parse_a_n_b_n:"
    pa "aaabbb"
    pa "ab"
    pa ""
    pa "aaa"
    pa "aab"
    pa "abb"
    pa "bbb"
    pa "abc"
    lf
    pu "parse_a_b_a_b:"
    pb "aabb"
    pb "aabb ab aabb"
    pb "ab ab  aabb"
    pb "ab aabb aaabbb aaaabbbb"
    lf