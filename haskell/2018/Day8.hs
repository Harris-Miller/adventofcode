module Day8 where

import Data.Graph

main' :: IO ()
main' = do
  contents <- map read . words . head . lines <$> readFile "../inputs/2018/Day8/input.txt" :: IO [Int]
  mapM_ print contents
