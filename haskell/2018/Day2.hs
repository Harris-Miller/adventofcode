module Day2 where

import Data.Function
import Data.List
import Data.Maybe

toTuple :: [Int] -> (Int, Int)
toTuple xs = (if 2 `elem` xs then 1 else 0, if 3 `elem` xs then 1 else 0)

process :: [String] -> Int
process xs = go xs (0, 0)
  where
    go [] (a, b) = a * b
    go (x : xs) (a, b) = go xs (a + a', b + b')
      where
        (a', b') = (toTuple . filter (> 1) . map length . group . sort) x

listsAgainstOthers :: [String] -> [(String, [String])]
listsAgainstOthers [] = []
listsAgainstOthers (x : xs) = (x, xs) : listsAgainstOthers xs

checkSimilarities :: String -> String -> String
checkSimilarities a b = catMaybes $ zipWith (\a b -> if a == b then Just a else Nothing) a b

process2 :: [(String, [String])] -> [String]
process2 = concatMap (\(s, xs) -> map (checkSimilarities s) xs)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2018/inputs/Day2/input.txt"
  print $ process contents
  -- part 2
  let r = process2 $ listsAgainstOthers contents
  print $ maximumBy (compare `on` length) r
  return ()
