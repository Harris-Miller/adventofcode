module Day20 where

import Data.Int.Factors
import Data.List

input = 33100000 :: Int

numPresentsDelivered :: Int -> Int
numPresentsDelivered = sum . map (* 10) . allFactors

numPresentsDelivered2 :: Int -> Int
numPresentsDelivered2 n = (sum . map (* 11) . filter ((<= 50) . (n `div`)) . allFactors) n

main' :: IO ()
main' = do
  -- part 1
  print $ find ((input <=) . numPresentsDelivered) [1 ..]
  -- part 2
  print $ find ((input <=) . numPresentsDelivered2) [1 ..]
  return ()
