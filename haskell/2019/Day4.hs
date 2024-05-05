module Day4 where

import Data.Char
import Data.List
import Data.List.Split

puzzleMin = 124075 :: Int

puzzleMax = 580769 :: Int

isAscending :: Int -> Bool
isAscending n = check s
  where
    s = map digitToInt (show n)
    check [l, r] = l <= r
    check (l : r : xs) = (l <= r) && check (r : xs)

hasAdjacents :: Int -> Bool
hasAdjacents n = check s
  where
    s = map digitToInt (show n)
    check [l, r] = l == r
    check (l : r : xs) = (l == r) || check (r : xs)

hasAdjacents2 :: Int -> Bool
hasAdjacents2 n = any (\x -> length x == 2) g
  where
    s = map digitToInt (show n)
    g = group s

main' :: IO ()
main' = do
  -- part2
  let passwords = filter (\x -> isAscending x && hasAdjacents x) [puzzleMin .. puzzleMax]
  print $ length passwords
  -- part 2
  let passwords = filter (\x -> isAscending x && hasAdjacents2 x) [puzzleMin .. puzzleMax]
  print $ length passwords
