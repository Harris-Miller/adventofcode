module Day2 where

import Data.Algebra.Boolean qualified as B
import Data.List.Split

parsePassword :: [String] -> (Int, Int, Char, String)
parsePassword [r, c, pw] = (lo, hi, head c, pw)
  where
    [lo, hi] = map read $ splitOn "-" r

validate :: (Int, Int, Char, String) -> Bool
validate (lo, hi, c, pw) = cs >= lo && cs <= hi
  where
    cs = length . filter (== c) $ pw

validate2 :: (Int, Int, Char, String) -> Bool
validate2 (lo, hi, c, pw) = (p1 == c) `B.xor` (p2 == c)
  where
    p1 = pw !! (lo - 1)
    p2 = pw !! (hi - 1)

main' :: IO ()
main' = do
  content <- map words . lines <$> readFile "../inputs/2020/Day2/input.txt"
  -- Part 1
  print $ length . filter id . map (validate . parsePassword) $ content
  -- Part 2
  print $ length . filter id . map (validate2 . parsePassword) $ content
