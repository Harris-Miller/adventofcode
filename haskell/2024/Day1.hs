module Day1 where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe

parse :: String -> [Int]
parse s =
  let [(a, s2)] = (reads :: ReadS Int) s
      [(b, _)] = (reads :: ReadS Int) s2
   in [a, b]

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2024/Day1/input.txt"
  let pairs = map parse contents

  let asLists = transpose pairs
  let sorted = (transpose . map sort) asLists
  let r1 = sum $ map (abs . foldr1 (-)) sorted
  print r1

  let [ll, rr] = asLists
  let rMap = foldr (\x -> M.insertWith (+) x 1) M.empty rr
  let r2 = sum $ mapMaybe (\x -> (* x) <$> M.lookup x rMap) ll
  print r2
