module Day17 where

import Algorithm.Search
import Control.Arrow
import Data.Char
import Data.Grid.Map
import qualified Data.Map as M
import Data.Maybe

data Direction = North | East | South | West
  deriving (Show, Eq, Ord)

data Move = Move {position :: (Int, Int), direction :: Direction, blocks :: Int}
  deriving (Show, Eq, Ord)

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

updatePos :: Direction -> (Int, Int) -> (Int, Int)
updatePos North = first (-1 +)
updatePos East = second (1 +)
updatePos South = first (1 +)
updatePos West = second (-1 +)

makeNeighbors :: Int -> Int -> GridM Int -> Move -> [Move]
makeNeighbors min max grid (Move pos dir blocks) = if mustKeepDirection then filter ((== dir) . direction) go else go
  where
    go = (filter (\(Move pos _ blocks) -> M.member pos grid && blocks <= max) . map (\d -> Move (updatePos d pos) d (nextBlock d)) . filter (/= opposite dir)) [North, East, South, West]
    mustKeepDirection = 0 < blocks && blocks < min
    nextBlock d
      | d == dir = blocks + 1
      | otherwise = 1

process :: Int -> Int -> GridM Int -> (Int, Int) -> Move -> Maybe (Int, [Move])
process min max grid end@(endR, endC) = aStar genNeighbors cost heuristic isAtEnd
  where
    isAtEnd = (== end) . position
    heuristic m = let (c, r) = position m in abs (endR - r) + abs (endR - c) -- standard dist between 2 points
    genNeighbors = makeNeighbors min max grid
    cost _ = (grid M.!) . position

main' :: IO ()
main' = do
  contents <- readFile "2023/inputs/Day17/input.txt"
  let grid = parseGrid (const True) digitToInt contents
  let start = Move (0, 0) East 0
  let end = fst $ M.findMax grid

  -- part 1
  let r = fromJust $ process 0 3 grid end start
  print $ fst r
  -- mapM_ print $ snd r
  let route = M.fromList $ map (,'#') $ (0, 0) : map (\(Move pos _ _) -> pos) (snd r)
  -- putStrLn $ gridToStr (fst $ M.findMax route) (const ' ') id route

  -- part 2
  let r2 = fromJust $ process 4 10 grid end start
  print $ fst r2
  -- mapM_ print $ snd r
  let route2 = M.fromList $ map (,'#') $ (0, 0) : map (\(Move pos _ _) -> pos) (snd r2)
  -- putStrLn $ gridToStr (fst $ M.findMax route) (const ' ') id route2
  return ()
