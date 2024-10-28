module Day12 where

import Data.List
import Data.Maybe

parse :: [String] -> ([Char], [(String, Char)])
parse xs =
  let s = (drop 15 . head) xs
      xs' = map (\a -> (take 5 a, last a)) $ drop 2 xs
   in (s, xs')

applyRules :: [(String, Char)] -> [Char] -> [Char]
applyRules rules [] = []
applyRules rules xs =
  let sub = take 5 $ xs ++ repeat '.'
      found = maybe '.' snd (find ((== sub) . fst) rules)
   in found : applyRules rules (tail xs)

trim :: String -> String
trim = dropWhile (== '.') . reverse . dropWhile (== '.') . reverse

nextGen :: [(String, Char)] -> [Char] -> [Char]
nextGen rules s = trim $ applyRules rules (".." ++ s)

main' :: IO ()
main' = do
  (start, rules) <- parse . lines <$> readFile "../inputs/2018/Day12/sample.txt"
  let r = take 20 $ iterate' (nextGen rules) start
  mapM_ print r
