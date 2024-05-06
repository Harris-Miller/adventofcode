module Day15 where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

data Space = Sensor | Beacon | Air | Ping
  deriving (Eq)

parseSpace :: String -> Space
parseSpace "S" = Sensor
parseSpace "B" = Beacon
parseSpace "." = Air
parseSpace "#" = Ping

instance Show Space where
  show Sensor = "S"
  show Beacon = "B"
  show Air = "."
  show Ping = "#"

type Point = (Int, Int)

type Grid = M.Map Point Space

parse :: String -> (Point, Point)
parse s = (sensor, beacon)
  where
    xs = words s
    parse' = fst . head . reads . last . splitOn "="
    sensor = (parse' (xs !! 2), parse' (xs !! 3)) :: Point
    beacon = (parse' (xs !! 8), parse' (xs !! 9)) :: Point

createGrid :: [(Point, Point)] -> M.Map Point Space
createGrid things = M.union (M.fromList $ map (,Sensor) sensors) (M.fromList $ map (,Beacon) beacons)
  where
    sensors = map fst things
    beacons = map snd things

showGrid :: Grid -> [String]
showGrid grid =
  let keys = M.keys grid
      xs = map fst keys
      ys = map snd keys
      xRange = [minimum xs .. maximum xs]
      yRange = [minimum ys .. maximum ys]
   in map (\y -> concatMap (show . (\x -> fromMaybe Air $ M.lookup (x, y) grid)) xRange) yRange

range :: Int -> Int -> [Int]
range a b
  | a > b = reverse [b .. a]
  | otherwise = [a .. b]

connect :: Point -> Point -> [Point]
connect (x1, y1) (x2, y2) = zip (range x1 x2) (range y1 y2)

moveN :: Point -> Point
moveN (x, y) = (x, y - 1)

moveE :: Point -> Point
moveE (x, y) = (x + 1, y)

moveS :: Point -> Point
moveS (x, y) = (x, y + 1)

moveW :: Point -> Point
moveW (x, y) = (x - 1, y)

moveFuncs = [moveN, moveE, moveS, moveW]

neighbors :: Point -> [Point]
neighbors = sequence moveFuncs

rowToCheck = 2000000 :: Int

filterRows :: [Point] -> [Point]
filterRows = filter (\(_, y) -> y == rowToCheck)

grow :: Point -> Point -> [Point]
grow sensor beacon = go [] (neighbors sensor)
  where
    go acc points@[pn, pe, ps, pw] = if atEnd then rtn else go acc' (zipWith ($) moveFuncs points)
      where
        newPoints = connect pn pe <> connect pe ps <> connect ps pw <> connect pw pn
        atEnd = beacon `elem` newPoints
        acc' = acc <> newPoints
        rtn = filterRows $ delete beacon acc'

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2022/Day15/input.txt"
  let grid = createGrid contents
  let ping1 = uncurry grow $ head contents
  print $ length ping1

-- let pings = foldr1 S.union $ map (uncurry grow) contents
-- print $ S.size pings

-- let grid' = M.union (M.fromList (map (,Ping) (S.toList pings))) grid
-- let rowToCheck = 2000000
-- let row = M.filterWithKey (\p@(_, y) s -> y == rowToCheck && s == Ping) grid'
-- mapM_ print $ showGrid grid'
-- print $ M.size row
