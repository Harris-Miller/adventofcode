module Day8 where

import Control.Arrow
import Data.List.Split
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Tuple.Common

parse :: String -> (String, Map String (String, String))
parse s = (instructions, xs)
  where
    [instructions, rest] = splitOn "\n\n" s
    xs = M.fromList $ map (second (toTuple . splitOn ", " . tail . init) . toTuple . splitOn " = ") $ lines rest

instToFunc :: Char -> ((a, a) -> a)
instToFunc 'L' = fst
instToFunc 'R' = snd

process1 :: Map String (String, String) -> String -> Int
process1 m is = go 0 "AAA" is
  where
    go :: Int -> String -> String -> Int
    go acc current [] = go acc current is
    go acc current (x : xs) = if next == "ZZZ" then acc' else go acc' next xs
      where
        next = instToFunc x $ m ! current
        acc' = acc + 1

process2 :: Map String (String, String) -> (String -> Bool) -> String -> String -> Int
process2 m found start is = go 0 start is
  where
    go :: Int -> String -> String -> Int
    go acc current [] = go acc current is
    go acc current (x : xs) = if found next then acc' else go acc' next xs
      where
        next = instToFunc x $ m ! current
        acc' = acc + 1

main' :: IO ()
main' = do
  contents <- readFile "../inputs/2023/Day8/input.txt"
  let (is, m) = parse contents
  -- part 1
  print $ process1 m is
  -- part 2
  let starts = filter ((== 'A') . last) $ M.keys m
  let ends = map (\x -> process2 m ((== 'Z') . last) x is) starts
  print $ foldr lcm 1 ends
  return ()
