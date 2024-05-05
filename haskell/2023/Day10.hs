module Day10 where

import Control.Monad.RWS
import Data.Grid.Map
import Data.List
import qualified Data.Map as M
import Data.Maybe

filterStartOptions :: GridM Char -> (Int, Int) -> [(Int, Int)]
filterStartOptions grid pos@(l, c) = mapMaybe go ns
  where
    ns = zip [0 ..] [(l - 1, c), (l, c + 1), (l + 1, c), (l, c - 1)]
    go :: (Int, (Int, Int)) -> Maybe (Int, Int)
    go (0, n) = if (grid M.! n) `elem` ("|F7" :: String) then Just n else Nothing
    go (1, n) = if (grid M.! n) `elem` ("-J7" :: String) then Just n else Nothing
    go (2, n) = if (grid M.! n) `elem` ("|JL" :: String) then Just n else Nothing
    go (3, n) = if (grid M.! n) `elem` ("-J7" :: String) then Just n else Nothing

getNextPos :: GridM Char -> (Int, Int) -> (Int, Int) -> (Int, Int)
getNextPos grid from pos@(l, c) = head $ filter (/= from) $ go charAtPos
  where
    charAtPos = grid M.! pos
    go '|' = [(l - 1, c), (l + 1, c)]
    go '-' = [(l, c - 1), (l, c + 1)]
    go 'L' = [(l - 1, c), (l, c + 1)]
    go 'J' = [(l - 1, c), (l, c - 1)]
    go 'F' = [(l + 1, c), (l, c + 1)]
    go '7' = [(l + 1, c), (l, c - 1)]
    go e = error $ "Failed to getNextPost at point" ++ show pos ++ " with value " ++ [e]

processTheLoop :: GridM Char -> (Char -> Bool) -> (Int, Int) -> RWS () [((Int, Int), Char)] (Int, Int) ()
processTheLoop grid isFound pos = do
  prev <- get
  let next = getNextPos grid prev pos
  let val = fromJust (M.lookup next grid)
  tell [(next, val)]
  put pos
  if isFound val
    then return ()
    else processTheLoop grid isFound next

main' :: IO ()
main' = do
  contents <- readFile "2023/inputs/Day10/input.txt"
  let grid = parseGrid (const True) id contents
  let start = fst $ head $ M.toList $ M.filter (== 'S') grid
  let startNeighbors = filterStartOptions grid start
  -- print startNeighbors
  -- print $ map (grid M.!) startNeighbors
  let firstNext = head $ filter ((/= '.') . fromJust . flip M.lookup grid) startNeighbors
  let (_, w) = execRWS (processTheLoop grid (== 'S') firstNext) () start
  -- mapM_ print w
  print $ (length w + 1) `div` 2
  return ()
