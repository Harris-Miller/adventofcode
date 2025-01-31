module Day7 where

import Algorithm.Search
import Data.List
import Data.List.Split
import Data.Maybe

parse :: String -> (Int, [Int])
parse s =
  let [a, b] = splitOn ": " s
      nums = map read $ splitOn " " b
   in (read a, nums)

process :: [Int -> Int -> Int] -> (Int, [Int]) -> Maybe [(Int, Int)]
process ops (testValue, values) = bfs next found (head values, 0)
  where
    len = length values
    found (acc, idx) = acc == testValue && idx == len - 1
    next (acc, idx)
      | acc > testValue = []
      | idx + 1 >= len = []
      | otherwise = let ni = idx + 1 in map (\f -> (f acc (values !! ni), ni)) ops

main' :: IO ()
main' = do
  content <- map parse . lines <$> readFile "../inputs/2024/Day7/input.txt"
  -- print content
  let ops = [(+), (*)]
  let r1 = sum $ map fst $ filter (isJust . process ops) content
  print r1
  let ops2 = [(+), (*), \a b -> read $ show a ++ show b]
  let r2 = sum $ map fst $ filter (isJust . process ops2) content
  print r2
