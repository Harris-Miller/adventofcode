module Day25 where

import Data.List
import Data.Tuple.Select
import System.IO.Unsafe

nextValue :: Int -> Int
nextValue = (`mod` 33554393) . (* 252533)

place :: ((Int, Int), Int) -> ((Int, Int), Int)
place ((row, column), value) = (go, nextValue value)
  where
    go
      | row - 1 == 0 = (column + 1, 1)
      | otherwise = (row - 1, column + 1)

main' :: IO ()
main' = do
  let firstValue = 20151125 :: Int
  let row = 2947 :: Int
  let col = 3029 :: Int
  let infiniteList = iterate place ((1, 1), firstValue)
  let result = find ((== (row, col)) . sel1) infiniteList
  print result
