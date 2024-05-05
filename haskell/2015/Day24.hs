module Day24 where

import Data.List

go :: Int -> [Int] -> ([Int], Int)
go num xs = head $ sortOn snd $ map (\x -> (x, product x)) groups'
  where
    weight = sum xs `div` num
    groups = filter ((== weight) . sum) $ subsequences xs
    shortest = minimum $ map length groups
    groups' = filter ((== shortest) . length) groups

main' :: IO ()
main' = do
  contents <- map read . lines <$> readFile "2015/inputs/Day24/input.txt" :: IO [Int]
  print $ snd $ go 3 contents
  print $ snd $ go 4 contents
