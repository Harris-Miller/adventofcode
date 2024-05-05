module Day2 where

import Data.List

-- | part 1
move :: Char -> Int -> Int
move 'U' n
  | n `elem` [1, 2, 3] = n
  | otherwise = n - 3
move 'R' n
  | n `elem` [3, 6, 9] = n
  | otherwise = n + 1
move 'L' n
  | n `elem` [1, 4, 7] = n
  | otherwise = n - 1
move 'D' n
  | n `elem` [7, 8, 9] = n
  | otherwise = n + 3

process :: (Char -> Int -> Int) -> String -> Int -> Int
process f s i = foldl (flip f) i s

-- | part 2
move2 :: Char -> Int -> Int
move2 'U' n
  | n `elem` [5, 2, 1, 4, 9] = n
  | n `elem` [3, 13] = n - 2
  | n == 3 = 1
  | otherwise = n - 4
move2 'R' n
  | n `elem` [1, 4, 9, 12, 13] = n
  | otherwise = n + 1
move2 'L' n
  | n `elem` [1, 2, 5, 10, 13] = n
  | otherwise = n - 1
move2 'D' n
  | n `elem` [5, 10, 13, 12, 9] = n
  | n `elem` [1, 11] = n + 2
  | otherwise = n + 4

showKey :: Int -> String
showKey a
  | a < 10 = show a
  | a == 10 = "A"
  | a == 11 = "B"
  | a == 12 = "C"
  | a == 13 = "D"

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2016/Day2/input.txt"
  -- part 1
  let r = scanl (flip (process move)) 5 contents
  print $ intercalate "" $ map show $ tail r
  -- part 2
  let r2 = scanl (flip (process move2)) 5 contents
  print $ intercalate "" $ map showKey $ tail r2
