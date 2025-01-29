module Day5 where

import Data.List
import Data.List.Split
import Data.Ord
import Data.Tuple.Common

parseInput :: [String] -> ([(Int, Int)], [[Int]])
parseInput xs =
  let (a, b) = toTuple $ splitOn [""] xs
      rules = map (toTuple . map read . splitOn "|") a
      pages = map (map read . splitOn ",") b
   in (rules, pages)

inRightOrder :: (Int, Int) -> [Int] -> Bool
inRightOrder rule page =
  let indexes = tmap (`elemIndex` page) rule
      r = sequenceT indexes
   in case r of
        Nothing -> True
        Just (a, b) -> a < b

getMiddleValue :: [a] -> a
getMiddleValue xs = xs !! (length xs `div` 2)

main' :: IO ()
main' = do
  (rules, pages) <- parseInput . lines <$> readFile "../inputs/2024/Day5/input.txt"
  -- print rules
  -- print pages
  let (correctOrder, incorrectOrder) = partition (\page -> all (`inRightOrder` page) rules) pages
  -- print correctOrder
  let r1 = sum $ map getMiddleValue correctOrder
  print r1
  let updated = map (sortBy (\l r -> if any (\(ll, rr) -> ll == l && rr == r) rules then LT else GT)) incorrectOrder
  let r2 = sum $ map getMiddleValue updated
  print r2
