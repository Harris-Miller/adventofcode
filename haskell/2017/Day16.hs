module Day16 where

import Data.List
import Data.List.Split
import Data.Maybe

startingPosition :: [Char]
startingPosition = ['a' .. 'p']

oneBillion :: Int
oneBillion = 1_000_000_000

data Op = Spin Int | Exchange Int Int | Partner Char Char
  deriving (Show)

parse :: String -> Op
parse ('s' : i) = Spin (read i)
parse ('x' : s) = let [a, b] = splitOn "/" s in Exchange (read a) (read b)
parse ['p', a, '/', b] = Partner a b

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j ls = [get k x | (k, x) <- zip [0 ..] ls]
  where
    get k x
      | k == i = ls !! j
      | k == j = ls !! i
      | otherwise = x

doOp :: [Char] -> Op -> [Char]
doOp s (Spin i) = let (a, b) = splitAt (length s - i) s in b <> a
doOp s (Exchange i1 i2) = swapElementsAt i1 i2 s
doOp s (Partner a b) = swapElementsAt i1 i2 s
  where
    i1 = fromJust $ elemIndex a s
    i2 = fromJust $ elemIndex b s

main' :: IO ()
main' = do
  contents <- map parse . splitOn "," . head . lines <$> readFile "../inputs/2017/Day16/input.txt"
  -- part 1
  print $ foldl doOp startingPosition contents
  -- part 2
  let contents2 = take (oneBillion * length contents) $ cycle contents
  print $ foldl doOp startingPosition contents2
