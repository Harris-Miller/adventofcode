module Day14 where

import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Common

twoByTwo :: [a] -> [(a, a)]
twoByTwo = go []
  where
    go acc [a, b] = acc <> [(a, b)]
    go acc (a : b : xs) = go (acc <> [(a, b)]) (b : xs)

data Space = Rock | Sand | Air | Start
  deriving (Eq)

type Point = (Int, Int)

type Grid = M.Map Point Space

parseSpace :: String -> Space
parseSpace "#" = Rock
parseSpace "o" = Sand
parseSpace "." = Air
parseSpace "+" = Start

instance Show Space where
  show Rock = "#"
  show Sand = "o"
  show Air = "."
  show Start = "+"

parseLine :: String -> [Point]
parseLine = map (toTuple . map read . splitOn ",") . splitOn " -> "

drawRockLines :: [Point] -> [Point]
drawRockLines xs = nub $ concatMap go xs'
  where
    xs' = twoByTwo xs
    go :: (Point, (Int, Int)) -> [(Int, Int)]
    go ((x1, y1), (x2, y2))
      | x1 == x2 = [(x1, y') | y' <- [min y1 y2 .. max y1 y2]]
      | y1 == y2 = [(x', y1) | x' <- [min x1 x2 .. max x1 x2]]

createGrid :: [(Int, Int)] -> Grid
createGrid = M.fromList . flip zip (repeat Rock)

showGrid :: Grid -> [String]
showGrid grid =
  let keys = M.keys grid
      xs = map fst keys
      ys = map snd keys
      xRange = [minimum xs .. maximum xs]
      yRange = [minimum ys .. maximum ys]
   in map (\y -> concatMap (show . (\x -> fromMaybe Air $ M.lookup (x, y) grid)) xRange) yRange

start = (500, 0) :: (Int, Int)

getNextPlacement :: Int -> Grid -> Point -> Point
getNextPlacement maxY grid p@(x, y) = go $ map (\p -> (p,) $ fromMaybe Air $ M.lookup p grid) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
  where
    next p'@(_, y') = if y' == maxY then p' else getNextPlacement maxY grid p'
    go [(p', Air), _, _] = next p'
    go [_, (p', Air), _] = next p'
    go [_, _, (p', Air)] = next p'
    go _ = p

tick :: Int -> Grid -> Grid
tick maxY grid = M.insert nextP Sand grid
  where
    nextP = getNextPlacement maxY grid start

run :: Grid -> (Grid, Int)
run grid = go grid 0
  where
    maxY = ((+ 1) . maximum . map snd . M.keys) grid
    go :: Grid -> Int -> (Grid, Int)
    go grid count = if y == maxY then (grid, count) else go (M.insert nextP Sand grid) (count + 1)
      where
        nextP@(_, y) = getNextPlacement maxY grid start

run2 :: Grid -> (Grid, Int)
run2 grid = go grid 0
  where
    maxY = ((+ 2) . maximum . map snd . M.keys) grid
    go :: Grid -> Int -> (Grid, Int)
    go grid count = if nextP == start then (grid', count') else go grid' count'
      where
        nextP = (\(x, y) -> if y == maxY then (x, y - 1) else (x, y)) $ getNextPlacement maxY grid start
        grid' = M.insert nextP Sand grid
        count' = count + 1

main' :: IO ()
main' = do
  grid <- M.insert start Start . createGrid . concatMap (drawRockLines . parseLine) . lines <$> readFile "../inputs/2022/Day14/input.txt"
  -- mapM_ print $ showGrid grid

  -- putStrLn ""
  -- mapM_ print $ showGrid $ iterate tick grid !! 1
  -- let (grid', count) = run grid
  -- mapM_ print $ showGrid grid'
  -- print count

  -- part 2
  let (grid', count) = run2 grid
  mapM_ print $ showGrid grid'
  print count
