module Day8 where

import Data.Char
import Data.Grid.Map
import Data.Map qualified as M

isTreeVisibility :: Int -> Int -> GridM Int -> (Int, Int) -> Bool
isTreeVisibility rMax cMax grid (r, c) = any (all (< grid M.! (r, c))) dirs
  where
    dirs = map (map (grid M.!)) $ get4DirsFromPoint rMax cMax (r, c)

main' :: IO ()
main' = do
  contents <- readFile "../inputs/2022/Day8/input.txt"
  let grid = parseGrid (const True) digitToInt contents
  let ((rMax, cMax), _) = M.findMax grid

  -- part 1
  print $ length $ filter id $ map (isTreeVisibility rMax cMax grid) $ M.keys grid
  -- \$ M.keys grid
  -- part 2
  -- print $ maximum $ map (calculateScenicScore rMax cMax) forest

  return ()
