module Day1 where

import Data.Char
import Data.List

indexToCheck :: Int -> Int -> Int
indexToCheck l i = if i' >= l then i' - l else i'
  where
    h = l `div` 2
    i' = h + i

doAdd :: Char -> Char -> Int
doAdd a b = if a == b then digitToInt a else 0

calculate :: String -> Int
calculate s = go 0 (last s : s)
  where
    go acc [a, b] = acc + doAdd a b
    go acc (a : b : xs) = go (acc + doAdd a b) (b : xs)

calc2 :: String -> Int
calc2 s = foldl go 0 iRange
  where
    l = length s
    iRange = [0 .. l - 1]
    go acc i = acc + doAdd (s !! i) (s !! indexToCheck l i)

main' :: IO ()
main' = do
  content <- head . lines <$> readFile "../inputs/2017/Day1/input.txt"
  print $ calculate content
  print $ calc2 content
