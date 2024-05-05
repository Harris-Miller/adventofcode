module Day3 where

import Data.List
import qualified Data.Map as M

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y + 1)
move (x, y) '>' = (x + 1, y)
move (x, y) 'v' = (x, y - 1)
move (x, y) '<' = (x - 1, y)
move p _ = p

main' :: IO ()
main' = do
  contents <- readFile "2015/inputs/Day3/input.txt"
  let contents' = scanl move (0, 0) contents
  -- part 1
  let results = foldr (\x -> M.insertWith (+) x 1) (M.singleton (0, 0) 1) contents'
  print $ M.size results
  -- part 2
  let a = zip [0 ..] contents
  let (santa, robot) = partition (\(i, _) -> even i) a
  let santa' = scanl move (0, 0) $ map snd santa
  let robot' = scanl move (0, 0) $ map snd robot
  let results2 = foldr (\x -> M.insertWith (+) x 1) (M.singleton (0, 0) 2) santa'
  let results2' = foldr (\x -> M.insertWith (+) x 1) results2 robot'
  print $ M.size results2'
