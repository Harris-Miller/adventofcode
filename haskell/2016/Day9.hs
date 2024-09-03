module Day9 where

import Data.Bifunctor
import Data.List
import Data.Tuple.Common

parseMarker :: String -> ((Int, Int), String)
parseMarker xs = ((s, r), drop 1 rest)
  where
    Just i = elemIndex ')' xs
    (marker, rest) = splitAt i xs
    Just i2 = elemIndex 'x' marker
    (s, r) = tmap read $ second (drop 1) $ splitAt i2 marker

extend :: String -> ((Int, Int), String) -> (String, String)
extend acc ((sub, rep), s) = (reverse $ concat (replicate rep (take sub s)) ++ acc, drop sub s)

process1 :: String -> String
process1 = reverse . go []
  where
    go acc [] = acc
    go acc ('(' : xs) = uncurry go $ extend acc $ parseMarker xs
    go acc (x : xs) = go (x : acc) xs

extend2 :: ((Int, Int), String) -> String
extend2 ((sub, rep), s) = concat (replicate rep (take sub s)) ++ drop sub s

process2 :: String -> String
process2 = reverse . go []
  where
    go acc [] = acc
    go acc ('(' : xs) = go acc $ extend2 (parseMarker xs)
    go acc (x : xs) = go (acc <> [x]) xs

main' :: IO ()
main' = do
  contents <- head . lines <$> readFile "../inputs/2016/Day9/input.txt"
  print $ length $ process1 contents
  print $ length $ process2 contents
  return ()
