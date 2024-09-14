module Day3 where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

main' :: IO ()
main' = do
  byLines <- map (map read . words) . lines <$> readFile "../inputs/2016/Day3/input.txt" :: IO [[Int]]
  -- part 1
  let contents1 = map (sortBy (comparing Down)) byLines
  let result1 = length $ filter id $ map ((\(l, ss) -> l < sum ss) . fromJust . uncons) contents1
  print result1

  -- part 2
  let contents2 = (concatMap (map (sortBy (comparing Down)) . transpose) . chunksOf 3) byLines
  let result2 = length $ filter id $ map ((\(l, ss) -> l < sum ss) . fromJust . uncons) contents2
  print result2
  return ()
