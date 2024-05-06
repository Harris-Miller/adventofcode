module Data.Grid3.Common where

getNeighbors4 :: (Int, Int, Int) -> [(Int, Int, Int)]
getNeighbors4 (z, y, x) = [(z', y', x') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], z' <- [z - 1 .. z + 1], (z', y', x') /= (z, y, x)]
