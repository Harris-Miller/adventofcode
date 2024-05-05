module Day3 where

import Data.List
import Data.List.Split

type Point = (Int, Int)

data Direction = U | R | D | L
  deriving (Enum, Eq, Show)

toDirection :: Char -> Direction
toDirection 'U' = U
toDirection 'R' = R
toDirection 'D' = D
toDirection 'L' = L

type Move = (Direction, Int)

parse :: String -> Move
parse (x : xs) = (toDirection x, read xs)

range :: Int -> Int -> [Int]
range a b
  | a < b = [a .. b]
  | otherwise = reverse [b .. a]

makeLine :: Move -> Point -> [Point]
makeLine (U, l) (x, y) = [(x, y') | y' <- range (y + 1) (y + l)]
makeLine (R, l) (x, y) = [(x', y) | x' <- range (x + 1) (x + l)]
makeLine (D, l) (x, y) = [(x, y') | y' <- range (y - 1) (y - l)]
makeLine (L, l) (x, y) = [(x', y) | x' <- range (x - 1) (x - l)]

draw :: [Move] -> [Point]
draw xs = go xs [(0, 0)]
  where
    go [] ps = ps
    go (x : xs) ps = go xs (ps <> makeLine x (last ps))

getDistances :: [Point] -> [Int]
getDistances (_ : xs) = map (\(x, y) -> abs x + abs y) xs

main' :: IO ()
main' = do
  contents <- map (map parse . splitOn ",") . lines <$> readFile "../inputs/2019/Day3/input.txt"
  let wires = map draw contents
  let crosses = let [a, b] = wires in intersect a b
  print crosses
  print $ minimum $ getDistances crosses
