module Day16 where

import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple.Common

parse :: String -> [(String, Int)]
parse = map (second read . toTuple . splitOn ": ") . splitOn ", " . (!! 1) . splitOn ": "

showStat :: [(String, Int)] -> String
showStat xs = "[" ++ intercalate ", " (go [] xs) ++ "]"
  where
    go acc [] = acc
    go acc ((k, v) : xs) = go (acc <> ["(" ++ k ++ ", " ++ show v ++ ")"]) xs

part1ops :: [(String, Int -> Int -> Bool)]
part1ops = [("children", (==)), ("cats", (==)), ("samoyeds", (==)), ("pomeranians", (==)), ("akitas", (==)), ("vizslas", (==)), ("goldfish", (==)), ("trees", (==)), ("cars", (==)), ("perfumes", (==))]

part2ops :: [(String, Int -> Int -> Bool)]
part2ops = [("children", (==)), ("cats", (>)), ("samoyeds", (==)), ("pomeranians", (<)), ("akitas", (==)), ("vizslas", (==)), ("goldfish", (<)), ("trees", (>)), ("cars", (==)), ("perfumes", (==))]

sueToFind = [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

isSue :: [(String, Int -> Int -> Bool)] -> [(String, Int)] -> (Int, [(String, Int)]) -> Maybe Int
isSue ops mySue (other, xs) = if go [] xs then Just other else Nothing
  where
    go :: [Bool] -> [(String, Int)] -> Bool
    go acc [] = and acc
    go acc ((k, v) : xs) = go (fromMaybe False ((lookup k ops <*> pure v) <*> lookup k mySue) : acc) xs

main' :: IO ()
main' = do
  contents <- zip [1 ..] . map parse . lines <$> readFile "2015/inputs/Day16/input.txt"
  -- part 1
  let result1 = fromJust $ find (isJust . isSue part1ops sueToFind) contents
  print $ fst result1
  -- part 2
  let result2 = fromJust $ find (isJust . isSue part2ops sueToFind) contents
  print $ fst result2
