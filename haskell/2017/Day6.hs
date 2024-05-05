module Day6 where

import Control.Lens
import Data.List.Common

distribute :: Int -> [Int] -> [Int]
distribute i xs = foldr (\i' acc -> over (element i') (+ 1) acc) xs' (take v $ iterate (circularInc l) (circularInc l i))
  where
    l = length xs
    v = xs !! i
    xs' = set (element i) 0 xs

process :: [Int] -> [[Int]] -> (Int, [Int])
process xs acc = go acc (0, xs)
  where
    go acc (c, xs) = if next `elem` acc then (c', next) else go (next : acc) (c', next)
      where
        next = distribute (findMaxIndex xs) xs
        c' = c + 1

main' :: IO ()
main' = do
  contents <- map read . words <$> readFile "../inputs/2017/Day6/input.txt" :: IO [Int]
  let r1 = process contents []
  print r1
  let r2 = process (snd r1) [snd r1]
  print r2
