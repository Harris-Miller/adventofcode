module Day8.Forest where

import qualified Data.Map as M
import Day8.Grid
import System.IO.Unsafe

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p [] = []
takeWhileOneMore p (x : xs) =
  if p x
    then x : takeWhileOneMore p xs
    else [x]

data Tree = Tree
  { coord :: Coord,
    height :: Int,
    north, east, south, west :: [Cell]
  }

instance Show Tree where
  show (Tree (r, c) height north east south west) = "Tree (" <> show r <> "," <> show c <> ") :: " <> show height

type Forest = [Tree]

createTree :: Cell -> Grid -> Int -> Int -> Tree
createTree cell grid rMax cMax =
  let (coord, height) = cell
      (r, c) = coord
      -- these need to be directional from the tree
      -- that is why north and west have reverse's
      north = zip (reverse [0 .. r - 1]) (replicate r c)
      south = zip [r + 1 .. rMax] (replicate (rMax - r) c)
      west = zip (replicate c r) (reverse [0 .. c - 1])
      east = zip (replicate (cMax - c) r) [c + 1 .. cMax]
   in unsafePerformIO $ do
        -- print "Creating Tree"
        -- print cell
        -- print north
        -- print south
        -- print west
        -- print east
        return
          Tree
            { coord = coord,
              height = height,
              north = map (`getCellFromCoord` grid) north,
              east = map (`getCellFromCoord` grid) east,
              south = map (`getCellFromCoord` grid) south,
              west = map (`getCellFromCoord` grid) west
            }

gridToForest :: Grid -> Int -> Int -> Forest
gridToForest grid rMax cMax = M.elems $ M.mapWithKey (\c h -> createTree (c, h) grid rMax cMax) grid

directionMax :: (Tree -> [Cell]) -> Tree -> Int
directionMax dir tree = maximum $ map snd (dir tree)

calculateTreeVisibility :: Int -> Int -> Tree -> Bool
calculateTreeVisibility rMax cMax tree = isCellOnEdge (coord tree) rMax cMax || any ((height tree >) . (`directionMax` tree)) [north, south, west, east]

directionScore :: (Tree -> [Cell]) -> Tree -> Int
directionScore dir tree = length $ takeWhileOneMore ((height tree >) . snd) (dir tree)

calculateScenicScore :: Int -> Int -> Tree -> Int
calculateScenicScore rMax cMax tree =
  if isCellOnEdge (coord tree) rMax cMax
    then 0
    else product $ map (`directionScore` tree) [north, east, south, west]
