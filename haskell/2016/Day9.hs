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
extend acc ((sub, rep), s) = (concat (acc : replicate rep (take sub s)), drop sub s)

process1 :: String -> String
process1 = go []
  where
    go acc [] = acc
    go acc ('(' : xs) = uncurry go $ extend acc $ parseMarker xs
    go acc (x : xs) = go (acc <> [x]) xs

main' :: IO ()
main' = do
  contents <- head . lines <$> readFile "../inputs/2016/Day9/input.txt"
  -- print $ length $ process1 "ADVENT"
  -- print $ length $ process1 "A(1x5)BC"
  -- print $ length $ process1 "(3x3)XYZ"
  -- print $ length $ process1 "A(2x2)BCD(2x2)EFG"
  -- print $ length $ process1 "(6x1)(1x3)A"
  -- print $ length $ process1 "X(8x2)(3x3)ABCY"
  print $ length $ process1 contents
  return ()
