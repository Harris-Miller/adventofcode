module Data.Grid3.Map
  ( module Data.Grid3.Common,
    module Data.Grid3.Map,
  )
where

import Data.Grid3.Common
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Prelude hiding (pred)

type Grid3M a = M.Map (Int, Int, Int) a

roundTheWay :: Int -> Int
roundTheWay (-2) = 1
roundTheWay 2 = -1
roundTheWay a = a

get3NeighborsCyclical :: Grid3M a -> (Int, Int, Int) -> [((Int, Int, Int), a)]
get3NeighborsCyclical grid3 (z, y, x) = catMaybes $ zipWith (\k v -> fmap (k,) v) neighbors' (map (`M.lookup` grid3) neighbors')
  where
    neighbors = [(z', y', x') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], z' <- [z - 1 .. z + 1], (z', y', x') /= (z, y, x)]
    neighbors' = map (\(a, b, c) -> (roundTheWay a, roundTheWay b, roundTheWay c)) neighbors

toLayers :: (Show a) => Grid3M a -> [M.Map (Int, Int) a]
toLayers grid3 = map (\z -> M.fromList $ M.foldrWithKey (\(_, y, x) v acc -> ((y, x), v) : acc) [] $ M.filterWithKey (\(z', _, _) _ -> z == z') grid3) zs
  where
    zs = sort $ nub $ M.foldrWithKey (\(z, _, _) _ acc -> z : acc) [] grid3
