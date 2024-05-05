module Day9 where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple

type Travel = ((String, String), Int)

parse :: String -> Travel
parse s = ((from, to), read distance)
  where
    [locs, distance] = splitOn " = " s
    [from, to] = splitOn " to " locs

flipAndConcat :: [Travel] -> [Travel]
flipAndConcat xs = xs <> map (\(p, d) -> (swap p, d)) xs

getCost :: [Travel] -> String -> String -> Int
getCost xs from to = fromJust $ lookup (from, to) xs

calcCost :: [Travel] -> ([Travel] -> String -> String -> Int) -> Int -> [String] -> Int
calcCost locs getCost acc [from, to] = acc + getCost locs from to
calcCost locs getCost acc (from : to : xs) = calcCost locs getCost next (to : xs)
  where
    next = acc + getCost locs from to

main' :: IO ()
main' = do
  travels <- flipAndConcat . map parse . lines <$> readFile "2015/inputs/Day9/input.txt"
  let routes = permutations $ nub $ map (fst . fst) travels
  let distances = map (calcCost travels getCost 0) routes
  -- part 1
  print $ minimum distances
  -- part 2
  print $ maximum distances
