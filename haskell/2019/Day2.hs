module Day2 where

import Control.Monad.RWS
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import IntCode

expectedOutput = 19690720

findExpectedOutput :: [(Int, Int)] -> M.Map Int Int -> IO (Int, Int)
findExpectedOutput ((noun, verb) : pairs) m = do
  let m' = M.insert 2 verb $ M.insert 1 noun m
  let theM = processIntCode m'
  (results, _, _) <- runRWST theM () ([0], 0)
  let r = fromJust $ M.lookup 0 results
  if r == expectedOutput then return (noun, verb) else findExpectedOutput pairs m

main' :: IO ()
main' = do
  contents <- zip [0 ..] . map read . splitOn "," . head . lines <$> readFile "2019/inputs/Day2/input.txt" :: IO [(Int, Int)]
  -- part 1
  let m = M.fromList contents
  let m' = M.insert 2 2 $ M.insert 1 12 m
  let theM = processIntCode m'
  (results, i, _) <- runRWST theM () ([0], 0)
  print $ fromJust $ M.lookup 0 results
  -- part 2
  let combos = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
  (noun, verb) <- findExpectedOutput combos m
  print $ 100 * noun + verb
