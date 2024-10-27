module Data.Grid.HashMap
  ( module Data.Grid.Common,
    module Data.Grid.HashMap,
  )
where

import Data.Grid.Common
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Point

type Grid a = HashMap (Int, Int) a

parseGrid :: (Char -> Bool) -> (Char -> a) -> String -> Grid a
parseGrid filterChar parseChar = HM.fromList . collectGrid filterChar parseChar

parseGridAsIs :: String -> Grid Char
parseGridAsIs = parseGrid (const True) id

type GridHM p a = HashMap p a

gridHMToStr ::
  Point -> -- min
  Point -> -- maxes
  (Point -> Char) -> -- fill when missing
  (a -> Char) -> -- toChar
  GridHM Point a -> -- grid
  [Char]
gridHMToStr mins (Point maxR maxC) fillWith toChar grid = go mins
  where
    getVal :: Point -> Char
    getVal pos = maybe (fillWith pos) toChar (HM.lookup pos grid)
    go :: Point -> [Char]
    go (Point r c)
      | (r, c) == (maxR, maxC) = [getVal (Point r c)]
      | c > maxC = '\n' : go (Point (r + 1) 0)
      | otherwise = getVal (Point r c) : go (Point r (c + 1))
