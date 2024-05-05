module Day10 where

import Data.List

tick :: String -> String
tick v = concatMap next $ group v
  where
    next v = show (length v) ++ nub v

main' :: IO ()
main' = do
  content <- head . lines <$> readFile "../inputs/2015/Day10/input.txt"
  -- part 1
  print $ length $ iterate tick content !! 40
  -- part 2
  print $ length $ iterate tick content !! 50
