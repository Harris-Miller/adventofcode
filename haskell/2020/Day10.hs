module Day10 where

import Data.List
import Data.Maybe

makeDiff :: [Int] -> [Int]
makeDiff xs = zipWith (-) (tail xs) xs

count :: (Eq a) => a -> [a] -> Int
count c = length . filter (== c)

-- who the fuck figures out shit like this
tribonacci :: Int -> Int
tribonacci 1 = 1
tribonacci 2 = 2
tribonacci 3 = 4
tribonacci n = tribonacci (n - 1) + tribonacci (n - 2) + tribonacci (n - 3)

main' :: IO ()
main' = do
  content <- (0 :) . sort . map read . lines <$> readFile "../inputs/2020/Day10/input.txt" :: IO [Int]
  -- Part 1
  let xs = 0 : sort content <> [maximum content + 3]
  let diffs = makeDiff xs

  print $ count 1 diffs * count 3 diffs

  -- Part 2
  print $ (product . map (tribonacci . length) . filter (\x -> head x == 1) . group . makeDiff) xs
