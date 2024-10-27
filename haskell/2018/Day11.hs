module Day11 where

import Data.Char
import Data.Foldable (maximumBy)
import Data.Grid.HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Ord

puzzleInput :: Int
puzzleInput = 9810

calcPower :: (Int, Int) -> Int
calcPower (x, y) =
  let rackId = x + 10
      powerLevel = show $ (rackId * y + puzzleInput) * rackId
      powerLevel' = if length powerLevel < 3 then 0 else (digitToInt . last . take 3 . reverse) powerLevel
   in powerLevel' - 5

get3x3Power :: HashMap (Int, Int) Int -> (Int, Int) -> Int
get3x3Power grid (x, y) =
  let i3x3 = (x + 1, y + 1) : getNeighbors8 (x + 1, y + 1)
   in sum $ map (grid HM.!) i3x3

main' :: IO ()
main' = do
  let coords = [(x, y) | x <- [1 .. 300], y <- [1 .. 300]]
  let powerLevels = map calcPower coords
  let grid = HM.fromList $ zip coords powerLevels

  let toCheck = [(x, y) | x <- [1 .. 298], y <- [1 .. 209]]
  let powerLevel9000 = fst $ maximumBy (comparing snd) $ map (\c -> (c, get3x3Power grid c)) toCheck
  print powerLevel9000
  return ()
