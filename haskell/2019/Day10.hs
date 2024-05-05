module Day10 where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

printGrid :: M.Map (Int, Int) Char -> IO ()
printGrid grid = do
  let ys = map (map snd) $ groupBy (\(k1, _) (k2, _) -> fst k1 == fst k2) $ M.toList grid
  mapM_ print ys

parse :: [String] -> M.Map (Int, Int) Char
parse ys = M.fromList $ foldr (\(i, xs) acc -> foldr (\(j, x) acc -> ((i, j), x) : acc) acc $ zip [0 ..] xs) [] $ zip [0 ..] ys

-- starting with 8-way cardinal directions for testing
angles1 :: [(Int, Int)]
--        N        NE       E       SE      S       SW       W        NW
angles1 = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

angles2 :: [(Int, Int)]
angles2 = [(-2, 1), (-1, 2), (1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1)]

angles3 :: [(Int, Int)]
angles3 = [(-3, 1), (-1, 3), (1, 3), (3, 1), (3, -1), (1, -3), (-1, -3), (-3, -1)]

angles = angles1 <> angles2 <> angles3

isAstroid :: Char -> Bool
isAstroid '#' = True
isAstroid _ = False

-- look in a direction, returns Nothing if reach edge of space
lookInDirection :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
lookInDirection grid look start = thing >>= go
  where
    next = move start look
    thing = M.lookup next grid
    go a = if a == '#' then Just next else lookInDirection grid look next

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2019/inputs/Day10/sample.txt"
  let grid = parse contents
  let as = M.filter (== '#') grid
  let found = length $ mapMaybe (\a -> lookInDirection grid a (3, 4)) angles
  print found
