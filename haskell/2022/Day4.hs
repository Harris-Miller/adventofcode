module Day4 where

import Data.List
import Data.List.Split

listToTuple2 :: [a] -> (a, a)
listToTuple2 [a, b] = (a, b)

t2fmap :: (a -> b) -> (a, a) -> (b, b)
t2fmap f (x, y) = (f x, f y)

getRange :: String -> [Int]
getRange str = [read x .. read y]
  where
    [x, y] = splitOn "-" str

doesOneIncludeTheOther :: ([Int], [Int]) -> Bool
doesOneIncludeTheOther (a, b) = all (`elem` a) b || all (`elem` b) a

lengthFilterTrue :: [Bool] -> Int
lengthFilterTrue = length . filter id

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2022/Day4/input.txt"
  let ranges = map (t2fmap getRange . listToTuple2 . splitOn ",") contents
  -- part 1
  print $ lengthFilterTrue $ map doesOneIncludeTheOther ranges
  -- part 2
  print $ lengthFilterTrue $ map (not . null . uncurry intersect) ranges
