module Day15 where

import Data.HashSet qualified as HS
import Data.List

type Point = (Int, Int)

parse :: String -> (Point, Point)
parse s =
  let [_, _, sx, sy, _, _, _, _, bx, by] = words s
      sx' = (fst . head . reads . drop 2) sx :: Int
      sy' = (fst . head . reads . drop 2) sy :: Int
      bx' = (fst . head . reads . drop 2) bx :: Int
      by' = (fst . head . reads . drop 2) by :: Int
   in ((sx', sy'), (bx', by'))

determineQuadrant :: (Point, Point) -> Int
determineQuadrant ((sx, sy), (bx, by))
  | sx < bx && sy >= by = 1
  | sx <= bx && sy < by = 2
  | sx > bx && sy <= by = 3
  | sx >= bx && sy > by = 4

findR :: Int -> (Point, Point) -> Int
findR q ((sx, sy), (bx, by)) =
  let go 1 = (bx - sx) + (sy - by)
      go 2 = (bx - sx) + (by - sy)
      go 3 = (sx - bx) + (by - sy)
      go 4 = (sx - bx) + (sy - by)
   in abs $ go q

getCorners :: Int -> Point -> (Point, Point, Point, Point)
getCorners r (x, y) = ((x, y - r), (x + r, y), (x, y + r), (x - r, y))

determineSensorDiamond :: (Point, Point) -> (Point, Point, Point, Point)
determineSensorDiamond (sensor, beacon) =
  let q = determineQuadrant (sensor, beacon)
      r = findR q (sensor, beacon)
   in getCorners r sensor

pointsForRow :: Int -> (Point, (Point, Point, Point, Point)) -> [Point]
pointsForRow row ((x, y), ((tx, ty), (rx, ry), (bx, by), (lx, ly)))
  | row < ty || by < row = []
  | row < y = let d = y - row in [(x', row) | x' <- [(lx + d) .. (rx - d)]]
  | otherwise = let d = row - y in [(x', row) | x' <- [(lx + d) .. (rx - d)]]

main' :: IO ()
main' = do
  content <- map parse . lines <$> readFile "../inputs/2022/Day15/input.txt"
  let (allSensors, allBeacons) = unzip content
  let allCorners = zip allSensors $ map determineSensorDiamond content

  -- let magicRow = 10
  let magicRow = 2000000

  let pointsForMagicRow = concatMap (pointsForRow magicRow) allCorners
  let answer = length $ HS.fromList pointsForMagicRow `HS.difference` HS.fromList allBeacons
  print answer

  return ()
