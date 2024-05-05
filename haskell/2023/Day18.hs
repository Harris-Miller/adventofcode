module Day18 where

import Data.List
import Data.List.Split
import Numeric

data DigPlan = DigPlan {direction :: Char, amount :: Int}
  deriving (Show)

parse :: String -> (DigPlan, String)
parse s = (DigPlan {direction = head dir, amount = read a}, (tail . init . tail) c)
  where
    [dir, a, c] = splitOn " " s

getDir :: Char -> Char
getDir '0' = 'R'
getDir '1' = 'D'
getDir '2' = 'L'
getDir '3' = 'U'

parseColor :: String -> DigPlan
parseColor color = DigPlan {direction = getDir (last color), amount = (fst . head . readHex . init) color}

fromOrigin :: DigPlan -> (Int, Int)
fromOrigin (DigPlan 'U' a) = (-a, 0)
fromOrigin (DigPlan 'R' a) = (0, a)
fromOrigin (DigPlan 'D' a) = (a, 0)
fromOrigin (DigPlan 'L' a) = (0, -a)

polygonInnerArea :: [(Int, Int)] -> Int
polygonInnerArea xs = sum [x1 * y2 - x2 * y1 | (y1, x1) : (y2, x2) : _ <- tails xs] `quot` 2

polygonArea :: [(Int, Int)] -> Int
polygonArea ps = abs (polygonInnerArea path) + perimeter `quot` 2 + 1
  where
    path = scanl1 (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) ps
    perimeter = sum [abs x + abs y | (x, y) <- ps]
    a = (sum . go) ps
    go [(x1, y1)] = let (x2, y2) = head ps in [(x1 * y2) - (y1 * x2)]
    go ((x1, y1) : x@(x2, y2) : xs) = (x1 * y2) - (y1 * x2) : go (x : xs)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2023/inputs/Day18/input.txt"
  let plans = map parse contents
  -- part 1
  let points = map (fromOrigin . fst) plans
  let a = polygonArea points
  print a

  -- part 2
  let plans2 = map (parseColor . snd) plans
  let points2 = map fromOrigin plans2
  let a2 = polygonArea points2
  print a2
  return ()
