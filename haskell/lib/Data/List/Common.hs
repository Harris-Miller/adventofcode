module Data.List.Common where

import Data.List
import Data.Maybe

circularInc :: Int -> Int -> Int
circularInc max i = i `rem` max

findMaxIndex :: [Int] -> Int
findMaxIndex xs = fromJust $ elemIndex (maximum xs) xs

pairs :: [a] -> [(a, a)]
pairs [_] = []
pairs (x : xs) = [(x, x') | x' <- xs] <> pairs xs

pairsOrdered :: [a] -> [(a, a)]
pairsOrdered xs = go xs
  where
    go [] = []
    go (x : rest) = [(x, x') | x' <- xs] <> go rest

combos :: [a] -> [(a, a)]
combos [x1, x2] = [(x1, x2)]
combos (x1 : x2 : xs) = (x1, x2) : combos (x2 : xs)
