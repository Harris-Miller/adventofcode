module Day18 where

import Data.List.Split

main' :: IO ()
main' = do
  contents <- map (map read . splitOn ",") . lines <$> readFile "../inputs/2022/Day18/sample.txt" :: IO [[Int]]
  print "lol fuck this one"
  return ()
