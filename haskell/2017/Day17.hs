module Day17 where

import Data.List
import Data.List.Common
import Data.Maybe

puzzleInput :: Int
puzzleInput = 337

spin :: Int -> [Int] -> [Int]
spin by xs = b <> a
  where
    by' = circularInc (length xs) by
    (a, b) = splitAt by' xs

main' :: IO ()
main' = do
  let spinBy = spin puzzleInput
  let r = foldl (\acc x -> (<> [x]) $ spinBy acc) [0] [1 .. 2017]
  print $ head r
  -- part 2
  let r2 = foldl (\acc x -> (<> [x]) $ spinBy acc) [0] [1 .. 50000000]
  let i = fromJust $ elemIndex 0 r2
  print $ r2 !! i + 1
