module Day1 where

import Data.List

parse :: Char -> Int
parse '(' = 1
parse ')' = -1
parse _ = 0

findPosition :: [Char] -> Int
findPosition xs = go $ zip [0 ..] $ scanl' (+) 0 $ map parse xs
  where
    go :: [(Int, Int)] -> Int
    go [] = error "did not find -1"
    go ((i, v) : xs) = if v == -1 then i else go xs

main' :: IO ()
main' = do
  contents <- head . lines <$> readFile "../inputs/2015/Day1/input.txt"
  -- part 1
  print $ sum $ map parse contents
  -- part 2
  print $ findPosition contents
