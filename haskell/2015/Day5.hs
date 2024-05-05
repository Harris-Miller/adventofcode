module Day5 where

import Data.List

-- part 1 rules

contains3Vowels :: String -> Bool
contains3Vowels = (> 2) . length . filter (not . null . flip intersect "aeiou" . singleton)

hasNoDisallowed :: String -> Bool
hasNoDisallowed s = not $ any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

hasDoubles :: String -> Bool
hasDoubles = any ((> 1) . length) . group

tests = [hasNoDisallowed, contains3Vowels, hasDoubles]

-- part 2 rules

groupsOfX :: [a] -> Int -> [[a]]
groupsOfX s x = go [] s
  where
    go acc xs
      | length xs == x = acc <> [xs]
      | otherwise = go (acc <> [group]) (drop 1 xs)
      where
        group = take x xs

matchingPairs :: String -> Bool
matchingPairs s = any ((>= 2) . length) $ group $ sort $ flip groupsOfX 2 $ concatMap shortenThrees $ group s
  where
    shortenThrees [x, _, _] = [x, x]
    shortenThrees xs = xs

hasAba :: String -> Bool
hasAba s = any (\[a, _, b] -> a == b) xs'
  where
    xs' = groupsOfX s 3

tests2 = [matchingPairs, hasAba]

-- main

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2015/Day5/input.txt"
  -- part 1
  print $ length $ filter (and . sequence tests) contents
  -- part 2
  print $ length $ filter (and . sequence tests2) contents
