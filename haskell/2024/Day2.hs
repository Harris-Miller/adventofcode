module Day2 where

import Data.Bifunctor
import Data.List
import Data.List.Common
import Data.Maybe

allAreIncreasing :: [Int] -> Bool
allAreIncreasing = all (\(l, r) -> l + 1 <= r && l + 3 >= r) . combos

areAllDecreasing :: [Int] -> Bool
areAllDecreasing = all (\(l, r) -> l - 1 >= r && l - 3 <= r) . combos

deleteIndex :: Int -> [a] -> [a]
deleteIndex i = uncurry (<>) . second (drop 1) . splitAt i

tryAgainRemovingOneAtATime :: [Int] -> Bool
tryAgainRemovingOneAtATime list =
  let lists = map (`deleteIndex` list) [0 .. length list]
   in isJust $ find (\line -> allAreIncreasing line || areAllDecreasing line) lists

main' :: IO ()
main' = do
  contents <- map (map read . words) . lines <$> readFile "../inputs/2024/Day2/input.txt" :: IO [[Int]]
  -- print contents

  -- part 1
  let r1 = length $ filter id $ map (\line -> allAreIncreasing line || areAllDecreasing line) contents
  print r1

  -- part 2
  let r2 = length $ filter id $ map (\line -> allAreIncreasing line || areAllDecreasing line || tryAgainRemovingOneAtATime line) contents
  print r2
