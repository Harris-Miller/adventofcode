module Day3 where

import Data.List
import Data.List.Split

data Claim = Claim {idNum :: String, x, y, w, h :: Int}
  deriving (Show)

toGrids :: Claim -> [(Int, Int)]
toGrids claim = [(x', y') | x' <- [x claim .. (x claim + y claim - 1)], y' <- [y claim .. (y claim + h claim - 1)]]

parse :: String -> Claim
parse s = Claim {idNum = tail id, x = read x, y = read (init y), w = read w, h = read h}
  where
    [id, _, xy, wh] = words s
    [x, y] = splitOn "," xy
    [w, h] = splitOn "x" wh

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2018/Day3/input.txt"
  -- part 1
  let overlaps = map head $ filter ((> 1) . length) $ group $ sort $ concatMap toGrids contents
  print $ length overlaps
  -- part 2, needs improvement
  let noOverlaps = filter (null . flip intersect overlaps . toGrids) contents
  print $ idNum $ head noOverlaps
