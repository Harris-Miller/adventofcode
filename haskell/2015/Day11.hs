module Day11 where

import Data.List
import Data.Maybe

-- Parsing

nextLetter :: Char -> Char
nextLetter 'z' = 'a'
nextLetter x = succ x

skipIllegals :: String -> Maybe String
skipIllegals s = (\(x, y) -> x ++ [nextLetter $ head y] ++ replicate (length y - 1) 'a') . flip splitAt s <$> findIndex (`elem` ("iol" :: String)) s

go :: String -> String -> String
go xs1 (x : xs2) = if x == 'a' then go (init xs1) $ nextLetter x : xs2 else xs1 ++ [x] ++ xs2

countWordUp :: String -> String
countWordUp xs = fromMaybe (go (init xs) [nextLetter $ last xs]) (skipIllegals xs)
  where
    go xs1 (x : xs2) = if x == 'a' then go (init xs1) $ nextLetter (last xs1) : x : xs2 else xs1 ++ [x] ++ xs2

-- Rules
make3Succ :: Char -> String
make3Succ = take 3 . iterate succ

has3Succ :: String -> Bool
has3Succ [a, b, c] = make3Succ a == [a, b, c]
has3Succ (a : b : c : xs) = make3Succ a == [a, b, c] || has3Succ (b : c : xs)

noIllegalChars :: String -> Bool
noIllegalChars = and . mapM notElem ("iol" :: String)

hasDoubles :: String -> Bool
hasDoubles = (> 1) . length . filter ((> 1) . length) . group

tests :: [String -> Bool]
tests = [has3Succ, noIllegalChars, hasDoubles]

findNextPassword :: String -> String
findNextPassword s = if and $ sequence tests s then s else findNextPassword $ countWordUp s

-- Main

main' :: IO ()
main' = do
  content <- head . lines <$> readFile "2015/inputs/Day11/input.txt"
  let results1 = findNextPassword content
  print results1
  print $ findNextPassword $ countWordUp results1
