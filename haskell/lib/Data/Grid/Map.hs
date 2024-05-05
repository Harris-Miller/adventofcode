module Data.Grid.Map
  ( module Data.Grid.Common,
    module Data.Grid.Map,
  )
where

import Control.Arrow
import Control.Monad.RWS
import Data.Grid.Common
import Data.Map (Map)
import qualified Data.Map as M

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
