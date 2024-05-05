module Day5 where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Range
import qualified Data.Set as S
import Data.Tuple.Common

parseInput :: [String] -> ([Int], [[(Int, Int, Int)]])
parseInput s = (seeds, groups)
  where
    seeds = map read $ splitOn " " $ drop 7 $ head s :: [Int]
    a = splitWhen null $ drop 2 s
    groups = map (map (toTuple3 . map read . splitOn " ") . drop 1) $ splitWhen null $ drop 2 s

convertInputViaRange :: (Int, Int, Int) -> Int -> Maybe Int
convertInputViaRange (destStart, srcStart, rangeLen) input = if inRange theRange input then Just converted else Nothing
  where
    theRange = srcStart +=* (srcStart + rangeLen)
    converted = input - srcStart + destStart

groupsToSrcToDestFunc :: [(Int, Int, Int)] -> Int -> Int
groupsToSrcToDestFunc rs input = go rangesToCheck
  where
    rangesToCheck = map convertInputViaRange rs
    go [] = input
    go (f : fs) = fromMaybe (go fs) (f input)

seedToLocation :: [[(Int, Int, Int)]] -> Int -> Int
seedToLocation groups seed = foldl' (\input f -> f input) seed groupFuncs
  where
    groupFuncs = map groupsToSrcToDestFunc groups

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2023/inputs/Day5/input.txt"
  -- part 1
  let (seeds, groups) = parseInput contents
  print $ minimum $ map (seedToLocation groups) seeds

  -- part 2
  let seedRanges = concatMap (\[start, len] -> [start .. start + len - 1]) $ chunksOf 2 seeds
  let locations = map (seedToLocation groups) seedRanges

  -- brute force is very slow, but does work
  print $ minimum locations

  return ()
