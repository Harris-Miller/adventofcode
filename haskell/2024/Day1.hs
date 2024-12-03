module Day1 where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe

parse :: String -> (Int, Int)
parse s =
  let [(a, s2)] = (reads :: ReadS Int) s
      [(b, _)] = (reads :: ReadS Int) s2
   in (a, b)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2024/Day1/input.txt"
  let pairs = map parse contents

  let (ll, rr) = unzip pairs
  let sorted = zip (sort ll) (sort rr)
  let r1 = sum $ map (abs . uncurry (-)) sorted
  print r1

  let rCounts = foldr (\x -> M.insertWith (+) x 1) M.empty rr
  let r2 = sum $ mapMaybe (\x -> (* x) <$> M.lookup x rCounts) ll
  print r2
