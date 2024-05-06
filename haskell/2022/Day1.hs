module Day1 where

import Data.List
import Data.List.Split
import Data.Ord

main' :: IO ()
main' = do
  r <- take 3 . sortBy (comparing Data.Ord.Down) . map (sum . map read) . splitWhen null . lines <$> readFile "../inputs/2022/Day1/input.txt"
  -- part 1
  print $ head r
  -- part 2
  print $ sum $ take 3 r
