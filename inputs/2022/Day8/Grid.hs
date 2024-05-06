module Day8.Grid where

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.IO.Unsafe

type Coord = (Int, Int)

type Cell = (Coord, Int)

type Grid = M.Map Coord Int

processRow :: Int -> String -> [Cell] -> [Cell]
processRow r xs acc = map (\(c, v) -> ((r, c), digitToInt v)) xsi <> acc
  where
    xsi = zip [0 .. length xs - 1] xs

contentsToGrid :: [String] -> Grid
contentsToGrid xs = M.fromList l
  where
    xsi = zip [0 .. length xs - 1] xs
    l = foldr (\(r, cs) acc -> processRow r cs acc) [] xsi

isCellOnEdge :: Coord -> Int -> Int -> Bool
isCellOnEdge (r, c) rMax cMax = r == 0 || r == rMax || c == 0 || c == cMax

getCellFromCoord :: Coord -> Grid -> Cell
getCellFromCoord coord grid = (coord, fromJust $ M.lookup coord grid)
