module Day8 where

import Data.Graph

main' :: IO ()
main' = do
  contents <- map read . words . head . lines <$> readFile "2018/inputs/Day8/input.txt" :: IO [Int]
  mapM_ print contents
