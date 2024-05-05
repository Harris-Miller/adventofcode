module Data.List.Common where

import Data.List
import Data.Maybe

circularInc :: Int -> Int -> Int
circularInc max i = i `rem` max

findMaxIndex :: [Int] -> Int
findMaxIndex xs = fromJust $ elemIndex (maximum xs) xs
