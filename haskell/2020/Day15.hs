module Day15 where

import Data.List
import qualified Data.Map as M
import Data.Maybe

sample :: [Int]
sample = [0, 3, 6]

input :: [Int]
input = [0, 1, 5, 10, 3, 12, 19]

--

wtf :: [Int] -> [Int]
wtf xs = xs <> [go indices]
  where
    lastSpoken = last xs
    indices = elemIndices lastSpoken (init xs)
    go [] = 0
    go is = length xs - last is - 1

main' :: IO ()
main' = do
  let content = input
  print $ last $ iterate wtf content !! (2020 - length content)
  return ()
