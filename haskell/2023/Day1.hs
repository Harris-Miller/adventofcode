module Day1 where

import Control.Monad
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

getFirstDigit :: String -> Char
getFirstDigit s = fromJust (find isDigit s)

getLastDigit :: String -> Char
getLastDigit = getFirstDigit . reverse

digitsAsText = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

textToCharMap = zip digitsAsText ['1' ..]

collectDigits :: String -> Writer [Char] ()
collectDigits [] = return ()
collectDigits s@(x : xs) = do
  when (isDigit x) (tell [x])
  tell $ map (\x -> fromJust $ lookup x textToCharMap) $ filter (`isPrefixOf` s) digitsAsText
  collectDigits xs

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2023/inputs/Day1/input.txt"
  -- print contents
  let part1 = (sum . map (read . (\s -> [getFirstDigit s, getLastDigit s]))) contents
  print part1
  let part2 = sum (map (read . (\x -> [head x, last x]) . (execWriter . collectDigits)) contents :: [Int])
  print part2
  return ()
