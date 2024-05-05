module Day8 where

import Data.Char

envelop :: (b -> [a] -> (b, [a])) -> b -> [a] -> b
envelop _ a [] = a
envelop f a xs = uncurry (envelop f) (f a xs)

countEscaped :: String -> Int
countEscaped = envelop countNext 0
  where
    countNext n "" = (n, "")
    countNext n ('\\' : '\\' : xs) = (n + 1, xs)
    countNext n ('\\' : '"' : xs) = (n + 1, xs)
    countNext n ('\\' : 'x' : d1 : d2 : xs)
      | isHexDigit d1 && isHexDigit d2 = (n + 1, xs)
      | otherwise = error "bad hex escape"
    countNext _ ('\\' : _) = error "bad backslash escape"
    countNext n (_ : xs) = (n + 1, xs)

countStringEscaped :: String -> Int
countStringEscaped s
  | head s == '"' && last s == '"' = countEscaped s - 2
  | otherwise = error "bad string"

countEscapesString :: String -> Int
countEscapesString = envelop countEscapes 2
  where
    countEscapes n "" = (n, "")
    countEscapes n ('"' : xs) = (n + 2, xs)
    countEscapes n ('\\' : xs) = (n + 2, xs)
    countEscapes n (_ : xs) = (n + 1, xs)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2015/inputs/Day8/input.txt"
  -- part 1
  let diff s = length s - countStringEscaped s
  print $ sum $ map diff contents
  -- part 2
  let diff' s = countEscapesString s - length s
  print $ sum $ map diff' contents
