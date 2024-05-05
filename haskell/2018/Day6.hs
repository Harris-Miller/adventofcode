module Day6 where

import Data.List.Split

type Point = (Int, Int)

parse :: String -> Point
parse s = (read x, read y)
  where
    [x, y] = splitOn ", " s

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2018/Day6/input.txt"
  mapM_ print contents
