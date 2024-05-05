module Day12 where

import Algorithm.Search
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe

parse :: String -> (Int, [Int])
parse s = (read program, map read $ splitOn ", " others)
  where
    [program, others] = splitOn " <-> " s

main' :: IO ()
main' = do
  contents <- M.fromList . map parse . lines <$> readFile "../inputs/2017/Day12/input.txt"
  let programs = M.keys contents
  let nextStates = fromJust . flip M.lookup contents
  let isFound = (== 0)
  let isFound' = map (==) programs
  let rs = mapMaybe (bfs nextStates isFound) programs
  let rs' = concat $ concatMap (\f -> mapMaybe (bfs nextStates f) programs) isFound'
  print $ length rs
  print $ length rs'
