module Day3 where

import Data.List

treesHit :: (Int, Int) -> [String] -> Int
treesHit (down, right) course = go 0 0 0
  where
    lRow = length course
    lCol = length (head course)
    go row col total = if row >= lRow then total else go (row + down) (col + right) newTotal
      where
        row' = course !! row
        col' = row' !! (col `mod` lCol)
        newTotal = total + if col' == '#' then 1 else 0

main' :: IO ()
main' = do
  content <- lines <$> readFile "../inputs/2020/Day3/input.txt"
  -- part 1
  print $ treesHit (1, 3) content
  -- part 2
  let moves = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  print $ product $ map (`treesHit` content) moves
