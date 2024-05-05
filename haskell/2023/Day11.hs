module Day11 where

import Data.Grid.Map
import Data.Map qualified as M

expandSpace :: (Int -> Int) -> [(Int, Int)] -> [(Int, Int)]
expandSpace expander grid = map (\(l, c) -> (l + expander (length $ filter (< l) ls'), c + expander (length $ filter (< c) cs'))) grid
  where
    (ls, cs) = unzip grid
    ((ll, cl), (lh, ch)) = ((minimum ls, minimum cs), (maximum ls, maximum cs))
    ls' = filter (`notElem` ls) [ll .. lh]
    cs' = filter (`notElem` cs) [cl .. ch]

findShortestToOthers :: [(Int, Int)] -> Int
findShortestToOthers grid = sum $ go grid
  where
    go :: [(Int, Int)] -> [Int]
    go [] = []
    go ((l, c) : xs) = map (\(ll, cc) -> abs (ll - l) + abs (cc - c)) xs <> go xs

main' :: IO ()
main' = do
  contents <- readFile "../inputs/2023/Day11/input.txt"
  let grid = M.keys $ parseGrid (== '#') id contents
  -- part 1
  let gridExpanded = expandSpace (* 1) grid
  print $ findShortestToOthers gridExpanded
  -- part 2
  let gridExpanded2 = expandSpace (* 999999) grid
  print $ findShortestToOthers gridExpanded2
  return ()
