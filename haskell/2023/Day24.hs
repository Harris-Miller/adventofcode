module Day24 where

import Data.List
import Data.List.Split
import Data.Point
import Data.Tuple.Common

parse :: String -> (Point3 Float, Point3 Float)
parse = tmap (fromTuple3 . toTuple3 . map read . splitOn ", ") . toTuple . splitOn " @ "

move :: (Point3 Float, Point3 Float) -> (Point3 Float, Point3 Float)
move (p, v) = (p <> v, v)

sampleTestArea :: (Int, Float)
sampleTestArea = (7, 27)

-- Point GetPointOfIntersection(Point p1, Point p2, Point n1, Point n2)
-- {
--     Point p1End = p1 + n1; // another point in line p1->n1
--     Point p2End = p2 + n2; // another point in line p2->n2

--     float m1 = (p1End.y - p1.y) / (p1End.x - p1.x); // slope of line p1->n1
--     float m2 = (p2End.y - p2.y) / (p2End.x - p2.x); // slope of line p2->n2

--     float b1 = p1.y - m1 * p1.x; // y-intercept of line p1->n1
--     float b2 = p2.y - m2 * p2.x; // y-intercept of line p2->n2

--     float px = (b2 - b1) / (m1 - m2); // collision x
--     float py = m1 * px + b1; // collision y

--     return new Point(px, py); // return statement
-- }

getPointOfIntersectionXYOnly :: (Point3 Float, Point3 Float) -> (Point3 Float, Point3 Float) -> Point Float
getPointOfIntersectionXYOnly (Point3 lx ly _, Point3 lvx lvy _) (Point3 rx ry _, Point3 rvx rvy _) = Point px py
  where
    lSlope = lvy / lvx
    rSlope = rvy / rvx
    bl = ly - lSlope * lx
    br = ry - rSlope * rx
    px = (br - bl) / (rSlope - lSlope)
    py = lSlope * px + bl

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2023/Day24/sample.txt"
  let (h1 : h2 : _) = contents
  print h1
  print h2
  let a = getPointOfIntersectionXYOnly h1 h2
  print a
  return ()
