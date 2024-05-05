module Day3 where

import Control.Monad
import Data.List
import Data.Maybe

puzzleInput = 277678 :: Int

spiral :: [(Int, Int)]
spiral = zip (walk 0 0 1) (walk 1 0 1)
  where
    walk n from to | to > 0 = replicate n from ++ [from, from + 1 .. to - 1] ++ walk (n + 1) to (0 - to)
    walk n from to | to < 0 = replicate n from ++ [from, from - 1 .. to + 1] ++ walk (n + 1) to (1 - to)

part2 :: (Num a) => [a] -> [a]
part2 sums = sums ++ [sum $ map (\n -> if n < length sums then sums !! n else 0) ns]
  where
    (x, y) = spiral !! length sums
    ns = map (fromJust . flip elemIndex spiral) $ liftM2 (,) [x - 1 .. x + 1] [y - 1 .. y + 1]

main' :: IO ()
main' = do
  let (x, y) = spiral !! (puzzleInput - 1)
  print $ abs x + abs y
  print $ last $ until ((> puzzleInput) . last) part2 [1]
