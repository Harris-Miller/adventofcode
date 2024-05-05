module Day23 where

import Algorithm.Search
import Control.Arrow
import Data.Grid.Map
import Data.List
import Data.Map (Map)
import Data.Map qualified as M

nextFromHill :: Char -> (Int, Int) -> [(Int, Int)]
nextFromHill '^' (r, c) = [(r - 1, c)]
nextFromHill '>' (r, c) = [(r, c + 1)]
nextFromHill 'v' (r, c) = [(r + 1, c)]
nextFromHill '<' (r, c) = [(r, c - 1)]

nextFromFlat :: GridM Char -> (Int, Int) -> [(Int, Int)]
nextFromFlat grid = filter (`M.member` grid) . getNeighbors4

next :: GridM Char -> (Int, Int) -> [(Int, Int)]
next grid p = if c == '.' then nextFromFlat grid p else nextFromHill c p
  where
    c = grid M.! p

main' :: IO ()
main' = do
  contents <- parseGridAsIs <$> readFile "../inputs/2023/Day23/sample.txt"
  let grid = M.filter (/= '#') contents
  let start = (0, 1)
  print $ M.findMax contents
  let end = second (-1 +) $ fst $ M.findMax contents

  print (start, end)

  let r = dijkstra (next grid) (\_ _ -> -1) (== end) start
  print r
  return ()
