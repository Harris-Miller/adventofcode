module Day18 where

import Data.Array.Unboxed
import Data.Grid.Array
import Data.List

toBool :: Char -> Bool
toBool '#' = True
toBool '.' = False

parse :: String -> GridUA Bool
parse = parseGrid (const True) toBool

getNeighbors :: GridUA Bool -> (Int, Int) -> [Bool]
getNeighbors grid pos = map (grid !) . filter (inRange (bounds grid)) $ getNeighbors8 pos

applyRule :: Bool -> [Bool] -> Bool
applyRule current neighbors
  | current = alive == 2 || alive == 3
  | otherwise = alive == 3
  where
    alive = (length . filter id) neighbors

tick :: GridUA Bool -> GridUA Bool
tick grid = array (bounds grid) $ map (\(k, v) -> (k, applyRule v (getNeighbors grid k))) (assocs grid)

cornersOn :: GridUA Bool -> GridUA Bool
cornersOn grid = accum (\_ x -> x) grid $ map (,True) [(0, 0), (0, c), (r, 0), (r, c)]
  where
    (_, (r, c)) = bounds grid

main' :: IO ()
main' = do
  grid <- parse <$> readFile "2015/inputs/Day18/input.txt"
  -- part 1
  print $ length $ filter id $ elems (iterate tick grid !! 100)
  -- part 2
  print $ length $ filter id $ elems (iterate (cornersOn . tick) (cornersOn grid) !! 100)
