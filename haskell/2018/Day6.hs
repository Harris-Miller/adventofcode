module Day6 where

import Data.List.Split
import Data.Point
import Data.Tuple.Common

parse :: String -> Point Int
parse s = Point x y
  where
    [x, y] = map read $ splitOn ", " s

findEncapsulations :: [Point Int] -> [(Point Int, [Point Int])]
findEncapsulations = undefined

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2018/Day6/sample.txt"
  mapM_ print contents
  print "Day 6"
