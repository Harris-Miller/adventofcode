{-# LANGUAGE RecordWildCards #-}

module Day15 where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List
import Data.Point

data ScanRange = ScanRange {c :: Point, r :: Int}

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

main' :: IO ()
main' = do
  content <- map parse . lines <$> readFile "../inputs/2022/Day15/input.txt"
  let (allSensors, allBeacons) = unzip content
  -- let allCorners = zip allSensors $ map determineSensorDiamond content

  -- let magicRow = 10
  let magicRow = 2000000

  let pointsForMagicRow = HS.fromList $ concatMap (pointsForRow magicRow . toScanRange) content
  let answer = length $ pointsForMagicRow `HS.difference` HS.fromList allBeacons
  print answer

  -- part 2
  -- let magicNumber2 = 20
  -- let magicNumber2 = 4000000
  -- let pointsForAllRows = HS.fromList $ concatMap (uncurry pointsForRow) [(r, c) | r <- [0 .. magicNumber2], c <- allCorners]
  -- let allPossiblePoints = HS.fromList $ [(x, y) | x <- [0 .. magicNumber2], y <- [0 .. magicNumber2]] :: HashSet (Int, Int)
  -- let (x, y) = head $ HS.toList $ allPossiblePoints `HS.difference` pointsForAllRows
  -- let answer2 = (x * 4000000) + y
  -- print answer2

  return ()
