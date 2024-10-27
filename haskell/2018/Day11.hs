module Day11 where

import Data.Array.Unboxed
import Data.Char
import Data.Foldable (maximumBy)
import Data.Ord

puzzleInput :: Int
puzzleInput = 9810

calcPower :: (Int, Int) -> Int
calcPower (x, y) =
  let rackId = x + 10
      powerLevel = show $ (rackId * y + puzzleInput) * rackId
      powerLevel' = if length powerLevel < 3 then 0 else (digitToInt . last . take 3 . reverse) powerLevel
   in powerLevel' - 5

getSizePower :: UArray (Int, Int) Int -> Int -> (Int, Int) -> Int
getSizePower grid size (x, y) =
  let square = [(x', y') | x' <- [x .. (x + size - 1)], y' <- [y .. (y + size - 1)]]
   in sum $ map (grid !) square

findMaxForGridSize :: UArray (Int, Int) Int -> Int -> (((Int, Int), Int), Int)
findMaxForGridSize grid size =
  let toCheck = [(x, y) | x <- [1 .. (300 - size + 1)], y <- [1 .. (300 - size + 1)]]
      powerLevel9000 = maximumBy (comparing snd) $ map (\c -> (c, getSizePower grid size c)) toCheck
   in (powerLevel9000, size)

main' :: IO ()
main' = do
  let coords = [(x, y) | x <- [1 .. 300], y <- [1 .. 300]]
  let powerLevels = map calcPower coords
  let grid = array ((1, 1), (300, 300)) $ zip coords powerLevels

  let part1 = findMaxForGridSize grid 3
  print part1

  -- part2
  let allPowerCombos = map (findMaxForGridSize grid) [1 .. 300]
  let part2 = maximumBy (comparing (snd . fst)) allPowerCombos
  print part2
  return ()
