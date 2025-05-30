module Day2 where

import Data.Bifunctor
import Data.List
import Data.List.Common
import Data.Maybe
import Data.Traversable

allAreIncreasing :: [Int] -> Bool
allAreIncreasing = all (\(l, r) -> l + 1 <= r && l + 3 >= r) . pairs

areAllDecreasing :: [Int] -> Bool
areAllDecreasing = all (\(l, r) -> l - 1 >= r && l - 3 <= r) . pairs

deleteIndex :: Int -> [a] -> [a]
deleteIndex i = uncurry (<>) . second (drop 1) . splitAt i

tryAgainRemovingOneAtATime :: [Int] -> Bool
tryAgainRemovingOneAtATime list =
  let lists = map (`deleteIndex` list) [0 .. length list]
      found = find (or . sequence [allAreIncreasing, areAllDecreasing]) lists
   in isJust found

validate :: [[Int] -> Bool] -> [[Int]] -> Int
validate funcs = length . filter (or . sequence funcs)

main' :: IO ()
main' = do
  contents <- map (map read . words) . lines <$> readFile "../inputs/2024/Day2/input.txt" :: IO [[Int]]
  -- print contents

  -- part 1
  let r1 = validate [allAreIncreasing, areAllDecreasing] contents
  print r1

  -- part 2
  let r2 = validate [allAreIncreasing, areAllDecreasing, tryAgainRemovingOneAtATime] contents
  print r2
