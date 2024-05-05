module Day1 where

import Control.Monad.State
import Data.Bifunctor
import Data.IntSet (IntSet)
import Data.IntSet qualified as S
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as M

parse :: String -> Int
parse ('+' : xs) = read xs
parse s = read s

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate list = go S.empty sums
  where
    sums = scanl (+) 0 (cycle list)
    go :: IntSet -> [Int] -> Int
    go s (x : xs) = if S.member x s then x else go (S.insert x s) xs

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2018/Day1/input.txt" :: IO [Int]
  -- part 1
  print $ sum contents
  -- part 2
  print $ findFirstDuplicate contents
  return ()
