{-# LANGUAGE RecordWildCards #-}

module Day15 where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List
import Data.Point

data ScanRange = ScanRange {c :: Point, r :: Int}
  deriving (Eq, Show)

parse :: String -> (Point, Point)
parse s =
  let go = (fst . head . reads . drop 2) :: String -> Int
      [_, _, sx, sy, _, _, _, _, bx, by] = words s
      sx' = go sx
      sy' = go sy
      bx' = go bx
      by' = go by
   in (Point sx' sy', Point bx' by')

toScanRange :: (Point, Point) -> ScanRange
toScanRange (sensor, beacon) =
  let Point w h = abs $ beacon - sensor
   in ScanRange {c = sensor, r = w + h}

pointsForRow :: Int -> ScanRange -> [Point]
pointsForRow row (ScanRange {..}) =
  let v = abs $ row - py c
      d = r - v
      cx = px c
   in [Point x' row | x' <- [(cx - d) .. (cx + d)]]

vonNeumannNeighborhood :: ScanRange -> [Point]
vonNeumannNeighborhood ScanRange {..} =
  let cx = px c
      cy = py c
      ps = [Point x y | x <- [cx - r .. cx + r], y <- [cy - r .. cy + r]]
   in filter (\(Point x y) -> abs (x - cx) + abs (y - cy) <= r) ps

createPerimeter :: Int -> Point -> [Point]
createPerimeter r (Point cx cy) =
  let s1 = [Point (cx + d) (cy + r - d) | d <- [0 .. r]]
      s2 = [Point (cx + d) (cy - r + d) | d <- [0 .. r]]
      s3 = [Point (cx - d) (cy + r - d) | d <- [0 .. r]]
      s4 = [Point (cx - d) (cy - r + d) | d <- [0 .. r]]
   in nub $ s1 <> s2 <> s3 <> s4

main' :: IO ()
main' = do
  content <- map parse . lines <$> readFile "../inputs/2022/Day15/input.txt"
  let (allSensors, allBeacons) = unzip content

  -- part 1

  -- let magicRow = 10
  let magicRow = 2000000

  let pointsForMagicRow = HS.fromList $ concatMap (pointsForRow magicRow . toScanRange) content
  let answer = length $ pointsForMagicRow `HS.difference` HS.fromList allBeacons
  print answer

  -- part 2

  -- let max = 20
  let max = 4000000

  let filterInRange = HS.filter (\(Point x y) -> x >= 0 && y >= 0 && x <= max && y <= max)

  let asScanRanges = map toScanRange content
  let allScannedPoints = HS.fromList $ concatMap vonNeumannNeighborhood asScanRanges
  let inRange = filterInRange allScannedPoints
  let pointsToExamine = filterInRange $ HS.fromList $ concatMap (\(ScanRange {..}) -> createPerimeter (r + 1) c) asScanRanges

  -- there has got to be a better way than calculating the taxicab circle and diffing against the perimeter
  let thePoint = head $ HS.toList $ pointsToExamine `HS.difference` inRange
  let answer2 = (\(Point x y) -> x * 4000000 + y) thePoint
  print answer2

  return ()
