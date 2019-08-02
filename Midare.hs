module Midare where

  import Prelude

  {-

  unlines :: String -> [String]
  unlines x = go id x
   where
    go :: ([String] -> [String]) -> String -> [String]
    go s [] = s [] : []
    go s (xv : xs) = case xv of
      '\n' -> s [] : go id xs
      _ -> go (\y -> s (xv : y)) xs

  lines :: [String] -> String
  lines [] = []
  lines (xv : xs) = go xv xs
   where
    go :: String -> [String] -> String
    go xv [] = xv
    go xv (xsv : xss) = case xv of
      [] -> '\n' : go xsv xss
      xvv : xvs -> xvv : go xvs (xsv : xss)

  -- lines . unlines

  unparagraphs :: [String] -> [[String]]
  unparagraphs x = go id x
   where
    go :: ([[String]] -> [[String]]) -> [String] -> [[String]]
    go s [] = s [] : []
    go s (xv : xs) = case xv of
      [] -> s [] : go id xs
      _ -> go (\y -> s (xv : y)) xs

  paragraphs :: [[String]] -> [String]
  paragraphs [] = []
  paragraphs (xv : xs) = go xv xs
   where
    go :: [String] -> [[String]] -> [String]
    go xv [] = xv
    go xv (xsv : xss) = case xv of
      [] -> [] : go xsv xss
      xvv : xvs -> xvv : go xvs (xsv : xss)

  -}

  data Lines = Cha Char Lines | New Break | End

  data Break = Break Lines

  unlines :: String -> Lines
  unlines x = go id x
   where
    go :: (Lines -> Lines) -> String -> [String]
    go s [] = s End
    go s (xv : xs) = case xv of
      '\n' -> s (New (Break (go id xs)))
      _ -> go (\y -> s (Cha xv y)) xs
