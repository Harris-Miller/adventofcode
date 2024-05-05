module Day9 where

import Data.List.Split

asPairs :: [a] -> [(a, a)]
asPairs [l, r] = [(l, r)]
asPairs (l : r : xs) = (l, r) : asPairs (r : xs)

process1 :: [Int] -> Int
process1 = sum . go []
  where
    go acc xs = if all (== 0) xs' then acc' else go acc' xs'
      where
        acc' = acc <> [last xs]
        xs' = map (\(l, r) -> r - l) $ asPairs xs

process2 :: [Int] -> Int
process2 = foldr1 (-) . go []
  where
    go acc xs = if all (== 0) xs' then acc' else go acc' xs'
      where
        acc' = acc <> [head xs]
        xs' = map (\(l, r) -> r - l) $ asPairs xs

main' :: IO ()
main' = do
  contents <- map (map read . splitOn " ") . lines <$> readFile "../inputs/2023/Day9/input.txt" :: IO [[Int]]
  -- part 1
  print $ sum $ map process1 contents
  -- part 2
  print $ sum $ map process2 contents
  return ()
