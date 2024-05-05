module Day17 where

import Data.List
import Data.List.Split

liters = 150 :: Int

main' :: IO ()
main' = do
  contents <- map read . lines <$> readFile "2015/inputs/Day17/input.txt" :: IO [Int]
  -- part 1
  let fillCombos = filter ((== liters) . sum) $ subsequences contents
  print $ length fillCombos

  -- part 2
  let min = (minimum . map length) fillCombos
  let howManyMin = length $ filter ((== min) . length) fillCombos
  print howManyMin
