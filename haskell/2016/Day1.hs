module Day1 where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple.Select

parse :: String -> (Char, Int)
parse (x : xs) = (x, read xs)

data Direction = North | East | South | West deriving (Show, Enum)

makeTurn :: Char -> Direction -> Direction
makeTurn 'R' West = North
makeTurn 'R' d = succ d
makeTurn 'L' North = West
makeTurn 'L' d = pred d

-- | Part 1
makeMove :: Int -> Direction -> (Int, Int) -> (Int, Int)
makeMove d North (x, y) = (x, y + d)
makeMove d East (x, y) = (x + d, y)
makeMove d South (x, y) = (x, y - d)
makeMove d West (x, y) = (x - d, y)

move :: (Direction, (Int, Int)) -> (Char, Int) -> (Direction, (Int, Int))
move (d, coords) (turn, steps) = (d', coords')
  where
    d' = makeTurn turn d
    coords' = makeMove steps d' coords

-- | Part 2
makeMove2 :: Direction -> (M.Map (Int, Int) Int, (Int, Int)) -> (M.Map (Int, Int) Int, (Int, Int))
makeMove2 d (visited, (x, y)) = (M.insertWith (+) pos' 1 visited, pos')
  where
    pos' = case d of
      North -> (x, y + 1)
      East -> (x + 1, y)
      South -> (x, y - 1)
      West -> (x - 1, y)

isDestination :: (Int, Int) -> M.Map (Int, Int) Int -> Bool
isDestination pos visited = (== 2) $ fromJust $ M.lookup pos visited

move2 :: [(Char, Int)] -> (Int, Int)
move2 moves = go moves North (M.singleton (0, 0) 1, (0, 0))
  where
    go :: [(Char, Int)] -> Direction -> (M.Map (Int, Int) Int, (Int, Int)) -> (Int, Int)
    go ((turn, steps) : xs) d (visited, pos) = if found then pos' else go xs d' (visited', pos')
      where
        d' = makeTurn turn d
        (visited', pos', found) = go' steps (visited, pos)
          where
            go' :: Int -> (M.Map (Int, Int) Int, (Int, Int)) -> (M.Map (Int, Int) Int, (Int, Int), Bool)
            go' 0 (visited, pos) = (visited, pos, False)
            go' steps (visited, pos) =
              let (visited', pos') = makeMove2 d' (visited, pos)
                  found = isDestination pos' visited'
               in if found then (visited', pos', found) else go' (steps - 1) (visited', pos')

main' :: IO ()
main' = do
  contents <- map parse . splitOn ", " <$> readFile "../inputs/2016/Day1/input.txt"
  -- part 1
  let result = foldl' move (North, (0, 0)) contents
  print $ (abs . fst . snd) result + (abs . snd . snd) result
  -- part 2
  let result2 = move2 contents
  print $ (abs . fst) result2 + (abs . snd) result2
