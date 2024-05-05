module Day4 where

import Data.List

listA = [1, 2, 3, 4, 5, 6]

doesEqual = 11

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates xs = go [] xs
  where
    go acc [] = False
    go acc (x : xs) = x `elem` acc || go (x : acc) xs

hasPermutations :: (Eq a) => [[a]] -> Bool
hasPermutations xs = go [] xs
  where
    go acc [] = False
    go acc (x : xs) = x `elem` acc' || go (x : acc) xs
      where
        predicates = sequence [(== length x) . length, (== length x) . length . intersect x]
        acc' = concatMap permutations $ filter (and . predicates) acc

main' :: IO ()
main' = do
  contents <- map words . lines <$> readFile "2017/inputs/Day4/input.txt"
  print $ length $ filter (not . hasDuplicates) contents
  print $ length $ filter (not . hasPermutations) contents
