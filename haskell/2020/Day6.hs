module Day6 where

import Data.List
import Data.List.Split

main' :: IO ()
main' = do
  content <- splitWhen (== "") . lines <$> readFile "../inputs/2020/Day6/input.txt"
  -- Part 1
  print $ sum $ map (length . nub . concat) content
  -- Part 2
  print $ sum $ map (length . foldl1 intersect . map nub) content
