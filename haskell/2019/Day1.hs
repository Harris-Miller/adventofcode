module Day1 where

import Data.List

calcFuel :: [Int] -> Int
calcFuel (x : xs) = if next == 0 then sum (x : xs) else calcFuel (next : x : xs)
  where
    next = max 0 ((x `div` 3) - 2)

main' :: IO ()
main' = do
  contents <- map read . lines <$> readFile "../inputs/2019/Day1/input.txt" :: IO [Int]
  let fuel = map (subtract 2 . (`div` 3)) contents
  let r = sum fuel
  print r
  let fuelForFuel = sum $ map (calcFuel . singleton) fuel
  print fuelForFuel
