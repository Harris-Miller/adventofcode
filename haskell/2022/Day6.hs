module Day6 where

import Data.List
import Data.List.Unique
import Data.Maybe

substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)

findFirstXUnique :: Int -> String -> Int
findFirstXUnique n contents = fromJust $ find (allUnique . fourChars) [n .. length contents - 1]
  where
    fourChars c = substring (c - n) c contents

main' :: IO ()
main' = do
  contents <- readFile "../inputs/2022/Day6/input.txt"
  -- part 1
  print $ findFirstXUnique 4 contents
  -- part 2
  print $ findFirstXUnique 14 contents
