module Day6 where

import Data.List
import Data.Tuple.Common

sample = [(7, 9), (15, 40), (30, 200)] :: [(Int, Int)]

input = [(57, 291), (72, 1172), (69, 1176), (92, 2026)] :: [(Int, Int)]

calcWins :: (Int, Int) -> Int
calcWins (t, d) = length $ filter (> d) $ map (\x -> (t - x) * x) [0 .. t]

main' :: IO ()
main' = do
  -- part 1
  print $ product $ map calcWins input
  -- part 2
  let actual = tmap (read . intercalate "" . map show) $ unzip input :: (Int, Int)
  print $ calcWins actual
