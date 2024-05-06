module Data.Grid.Map
  ( module Data.Grid.Common,
    module Data.Grid.Map,
  )
where

import Control.Arrow
import Control.Monad.RWS
import Data.Grid.Common
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Prelude hiding (pred)

type GridM a = Map (Int, Int) a

parseGrid :: (Char -> Bool) -> (Char -> a) -> String -> GridM a
parseGrid filterChar parseChar = M.fromList . collectGrid filterChar parseChar

parseGridAsIs :: String -> GridM Char
parseGridAsIs = parseGrid (const True) id

gridToStr ::
  (Int, Int) -> -- maxes
  ((Int, Int) -> Char) -> -- fill when missing
  (a -> Char) -> -- toChar
  GridM a -> -- grid
  [Char]
gridToStr (maxR, maxC) fillWith toChar grid = go (0, 0)
  where
    getVal pos = maybe (fillWith pos) toChar (M.lookup pos grid)
    go :: (Int, Int) -> [Char]
    go (r, c)
      | (r, c) == (maxR, maxC) = [getVal (r, c)]
      | c > maxC = '\n' : go (r + 1, 0)
      | otherwise = getVal (r, c) : go (r, c + 1)

gridCharToStr :: GridM Char -> String
gridCharToStr grid = gridToStr (fst $ M.findMax grid) (const ' ') id grid

genNeighbors8First :: ((Int, Int) -> Maybe Bool) -> (Int, Int) -> GridM a -> [(Int, Int)]
genNeighbors8First pred k grid = mapMaybe (find (fromMaybe False . pred)) $ filter (not . null) (genNeighbors8All k grid)

-- get neighbors continuously in 8 directions
genNeighbors8All :: (Int, Int) -> GridM a -> [[(Int, Int)]]
genNeighbors8All (c, r) grid = map (filter (/= (c, r))) [up, upRight, right, downRight, down, downLeft, left, upLeft]
  where
    (cMax, rMax) = (fst . M.findMax) grid
    up = map (,r) (reverse [0 .. c])
    upRight = zip (reverse [0 .. c]) [r .. rMax]
    right = map (c,) [r .. rMax]
    downRight = zip [c .. cMax] [r .. rMax]
    down = map (,r) [c .. cMax]
    downLeft = zip [c .. cMax] (reverse [0 .. r])
    left = map (c,) (reverse [0 .. r])
    upLeft = zip (reverse [0 .. c]) (reverse [0 .. r])
