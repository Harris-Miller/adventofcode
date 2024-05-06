module Day7 where

import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe

removeSuffix :: String -> String
removeSuffix s
  | " bag" `isSuffixOf` s = (reverse . drop 4 . reverse) s
  | " bags" `isSuffixOf` s = (reverse . drop 5 . reverse) s
  | otherwise = s

parseThing :: String -> [(Int, String)]
parseThing s = if "no" `isPrefixOf` s then [] else c
  where
    a = (splitOn ", " . init) s
    b = map (break (== ' ')) a
    c = map (bimap read (removeSuffix . drop 1)) b

recursiveContains :: String -> [(String, [(Int, String)])] -> [String] -> [String]
recursiveContains s m xs = if null thing then xs else foldr (`recursiveContains` m) (xs <> thing) thing
  where
    thing = mapMaybe (\(k, v) -> if s `elem` map snd v then Just k else Nothing) m

recursiveRequired :: String -> [(String, [(Int, String)])] -> Int
recursiveRequired s m = amounts + sum (map (`recursiveRequired` m) next)
  where
    a = snd $ fromJust $ find ((== s) . fst) m
    amounts = sum $ map fst a
    next = concatMap (uncurry replicate) a

main' :: IO ()
main' = do
  content <- lines <$> readFile "../inputs/2020/Day7/input.txt"
  -- Part 1
  let bags = map ((\[b, o] -> (removeSuffix b, parseThing o)) . splitOn " contain ") content
  -- mapM_ print $ bags
  print $ length $ nub $ recursiveContains "shiny gold" bags []
  -- part 2
  print $ recursiveRequired "shiny gold" bags
