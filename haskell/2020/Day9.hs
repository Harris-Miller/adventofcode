module Day9 where

import Data.List
import Data.Maybe

isSum :: Int -> [Int] -> Bool
isSum x xs = or [a + b == x | a <- xs, b <- xs, a /= b]

findFirstInvalid :: Int -> [Int] -> Int
findFirstInvalid pre xs = go $ pre + 1
  where
    go :: Int -> Int
    go i = if isSum x (take pre $ drop (i - pre) xs) then go (i + 1) else x
      where
        x = xs !! i

findContiguous :: Int -> [Int] -> [Int]
findContiguous x xs = go 0
  where
    is = [0 .. length xs - 1]
    a = nub $ concatMap (filter ((== x) . sum) . drop 1 . inits . flip drop xs) is
    go i = fromMaybe (go (i + 1)) ((find ((== x) . sum) . drop 1 . inits . flip drop xs) i)

main' :: IO ()
main' = do
  content <- map read . lines <$> readFile "../inputs/2020/Day9/input.txt" :: IO [Int]
  -- Part 1
  let invalidNum = findFirstInvalid 25 content
  print invalidNum

  -- Part 2
  let r = findContiguous invalidNum content
  print $ minimum r + maximum r
