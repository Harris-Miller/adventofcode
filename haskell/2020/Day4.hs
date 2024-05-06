module Day4 where

import Data.Char
import Data.List
import Data.List.Split

fields :: [String]
fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

filterPart1 :: (String, String) -> Bool
filterPart1 _ = True

parseTuple :: [String] -> (String, String)
parseTuple [a, b] = (a, b)

mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (a, b) = (f a, b)

filterPart2 :: (String, String) -> Bool
filterPart2 ("byr", year) = read year `elem` [1920 .. 2002]
filterPart2 ("iyr", year) = read year `elem` [2010 .. 2020]
filterPart2 ("eyr", year) = read year `elem` [2020 .. 2030]
filterPart2 ("hgt", height) = let (n, u) = mapFirst read $ span isDigit height :: (Int, String) in n `elem` (if u == "cm" then [150 .. 193] else [59 .. 76])
filterPart2 ("hcl", color) = maybe False (\(h, t) -> h == '#' && length t == 6 && all isHexDigit t) (uncons color)
filterPart2 ("ecl", color) = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
filterPart2 ("pid", pid) = length pid == 9 && all isDigit pid
filterPart2 ("cid", _) = True
filterPart2 _ = False

toTuple2 :: [String] -> (String, String)
toTuple2 [a, b] = (a, b)

main' :: IO ()
main' = do
  content <- map (map (parseTuple . splitOn ":") . concatMap (splitOn " ")) . splitWhen (== "") . lines <$> readFile "../inputs/2020/Day4/input.txt"
  -- part 1
  print $ length $ filter id $ map (\x -> all (`elem` map fst x) fields) content
  -- part 2
  print $ length $ filter id $ map (\x -> all (`elem` map fst x) fields && all filterPart2 x) content
