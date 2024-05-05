module Day5 where

import Data.Char

react :: String -> Int
react = length . foldr go ""
  where
    go x (y : ys) | x /= y && toUpper x == toUpper y = ys
    go x yx = x : yx

filterBy :: Char -> String -> String
filterBy c = filter (\x -> x /= c && x /= toUpper c)

main' :: IO ()
main' = do
  content <- head . lines <$> readFile "../inputs/2018/Day5/input.txt"
  -- part 1
  print $ react content
  -- part 2
  let alphabet = ['a' .. 'z']
  print $ minimum $ map (react . flip filterBy content) alphabet
