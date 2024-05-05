module Day19 where

import Data.List
import Data.List.Split
import Data.Tuple.Common

parse :: String -> (String, [(String, String)])
parse s = (mol, rs)
  where
    xs = lines s
    mol = last $ dropWhile (/= "") xs
    rs = map (toTuple . splitOn " => ") $ takeWhile (/= "") xs

replace :: (String, String) -> String -> [String]
replace (from, to) mol = go "" ss
  where
    l = length from
    ss = filter (not . null) $ split (keepDelimsL $ onSublist from) mol
    go :: String -> [String] -> [String]
    go acc [] = []
    go acc (x : xs)
      | from `isPrefixOf` x = (acc ++ to ++ drop l x ++ concat xs) : go (acc ++ x) xs
      | otherwise = (acc ++ x ++ concat xs) : go (acc ++ x) xs

main' :: IO ()
main' = do
  contents <- readFile "../inputs/2015/Day19/input.txt"
  let (mol, rs) = parse contents
  -- part 1 -- why is it giving me one more than it should?
  print $ length $ nub $ concatMap (`replace` mol) rs
  return ()
