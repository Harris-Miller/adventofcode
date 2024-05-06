module Day11 where

import Data.Foldable qualified as M
import Data.Grid.Map
import Data.Map qualified as M
import Data.Maybe

data Space = Floor | Occupied | Empty
  deriving (Eq)

parseSpace :: Char -> Space
parseSpace '.' = Floor
parseSpace 'L' = Empty
parseSpace '#' = Occupied
parseSpace _ = error "fuck"

instance Show Space where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

updateSeat :: (Int, Int) -> Space -> GridM Space -> Space
updateSeat k v m
  | v == Empty && notElem Occupied ns = Occupied
  | v == Occupied && length (filter (== Occupied) ns) >= 4 = Empty
  | otherwise = v
  where
    ns = mapMaybe (`M.lookup` m) (getNeighbors8 k)

updateSeat2 :: (Int, Int) -> Space -> GridM Space -> Space
updateSeat2 k v m
  | v == Empty && notElem Occupied ns = Occupied
  | v == Occupied && length (filter (== Occupied) ns) >= 5 = Empty
  | otherwise = v
  where
    ns = mapMaybe (`M.lookup` m) (genNeighbors8First (\k' -> (/= Empty) <$> M.lookup k' m) k m)

updateGrid :: ((Int, Int) -> Space -> GridM Space -> Space) -> GridM Space -> GridM Space
updateGrid updateSeatFn grid = M.mapWithKey (\k v -> updateSeatFn k v grid) grid

repeatUntilFull :: ((Int, Int) -> Space -> GridM Space -> Space) -> GridM Space -> GridM Space
repeatUntilFull updateSeatFn grid = if grid == next then next else repeatUntilFull updateSeatFn next
  where
    next = updateGrid updateSeatFn grid

main' :: IO ()
main' = do
  content <- readFile "../inputs/2020/Day11/sample.txt"
  -- Part 1
  let grid = parseGrid (const True) parseSpace content
  let endGrid = repeatUntilFull updateSeat grid
  print $ M.length $ M.filter (== Occupied) endGrid

  -- part 2
  let endGrid2 = repeatUntilFull updateSeat2 grid
  print $ M.length $ M.filter (== Occupied) endGrid2
