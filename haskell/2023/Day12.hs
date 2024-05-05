module Day12 where

import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import Data.Traversable
import Data.Tuple
import Data.Tuple.Common

parseLine :: String -> ([Int], String)
parseLine = swap . second (map read . splitOn ",") . toTuple . splitOn " "

badReplace :: String -> String -> String
badReplace xs [] = xs
badReplace [] _ = []
badReplace (x : xs) rss@(r : rs) = if x == '?' then r : badReplace xs rs else x : badReplace xs rss

processPermutations :: String -> [String]
processPermutations s = perms
  where
    n = length $ filter (== '?') s
    perms = map (`badReplace` s) $ replicateM n ['.', '#']

validate :: [Int] -> String -> Bool
validate nums s = (length nums == length ss) && all (\(n, xs) -> n == length xs) (zip nums ss)
  where
    ss = filter (notElem '.') $ group s

unfoldShit :: String -> String
unfoldShit s = concat ss'
  where
    ss = groupBy (\a b -> (a == '.' || a == '#') && (b == '.' || b == '#')) s
    ss' = map (\s' -> if s' == "?" then s' else intercalate (replicate (length s') '?') (replicate 5 s')) ss

process :: [([Int], String)] -> [Int]
process contents = r2
  where
    r = map (second (\s -> map (badReplace s) $ processPermutations s)) contents
    r2 = map (length . filter id . \(ns, ss) -> map (validate ns) ss) r

main' :: IO ()
main' = do
  contents <- map parseLine . lines <$> readFile "2023/inputs/Day12/sample.txt"
  -- part 1
  -- print $ process contents

  -- part 2
  let contents2 = map (concat . replicate 5 *** unfoldShit) contents
  let r3 = process contents2
  mapM_ print r3
  print $ sum r3
  return ()
