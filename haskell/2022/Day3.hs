module Day3 where

import Data.Function.Tools
import Data.List
import Data.List.Split
import Data.Maybe

priorities = zip (['a' .. 'z'] <> ['A' .. 'Z']) [1 ..]

getPriority :: Char -> Int
getPriority = fromJust . flip lookup priorities

splitAtMiddle :: String -> (String, String)
splitAtMiddle = apply2way splitAt ((`div` 2) . length) id

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2022/Day3/input.txt"
  -- part 1
  print $ sum $ map (getPriority . head . uncurry intersect . splitAtMiddle) contents
  -- part 2
  print $ sum $ map (getPriority . head . foldr1 intersect) $ chunksOf 3 contents
