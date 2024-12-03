module Data.List.Common where

import Data.List
import Data.Maybe

circularInc :: Int -> Int -> Int
circularInc max i = i `rem` max

findMaxIndex :: [Int] -> Int
findMaxIndex xs = fromJust $ elemIndex (maximum xs) xs

combos :: [a] -> [(a, a)]
combos [_] = []
combos (x : xs) = [(x, x') | x' <- xs] <> combos xs

combosInclusive :: [a] -> [(a, a)]
combosInclusive xs = go xs
  where
    go [] = []
    go (x : rest) = [(x, x') | x' <- xs] <> go rest

pairs :: [a] -> [(a, a)]
pairs [x1, x2] = [(x1, x2)]
pairs (x1 : x2 : xs) = (x1, x2) : pairs (x2 : xs)
