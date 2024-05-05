module Day2 where

import Data.List
import Data.List.Split

type Dimensions = [Int]

determineAreaNeeded :: [Int] -> Int
determineAreaNeeded [l, w, h] = sum sides + minimum sides
  where
    sides = [l * w, l * w, l * h, l * h, w * h, w * h]

determineLengthNeeded :: [Int] -> Int
determineLengthNeeded xs = product xs + length' xs
  where
    length' = sum . concatMap (\x -> [x, x]) . init . sort

main' :: IO ()
main' = do
  contents <- map (map read . splitOn "x") . lines <$> readFile "2015/inputs/Day2/input.txt" :: IO [[Int]]
  -- part 1
  print $ sum $ map determineAreaNeeded contents
  -- part 2
  print $ sum $ map determineLengthNeeded contents
