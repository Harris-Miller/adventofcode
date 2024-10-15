module Data.Grid.Common where

import Control.Monad.RWS
import Data.Point (Point (Point))

collectGrid :: (Char -> Bool) -> (Char -> a) -> String -> [((Int, Int), a)]
collectGrid filterChar parseChar s = snd $ execRWS (go filterChar parseChar s) () (0, 0)
  where
    go :: (Char -> Bool) -> (Char -> a) -> String -> RWS () [((Int, Int), a)] (Int, Int) ()
    go filterChar parseChar [] = return ()
    go filterChar parseChar (x : xs) = do
      (l, c) <- get
      if x == '\n'
        then put (l + 1, 0)
        else when (filterChar x) (tell [((l, c), parseChar x)]) >> put (l, c + 1)
      go filterChar parseChar xs

getNeighbors4 :: (Int, Int) -> [(Int, Int)]
getNeighbors4 (r, c) = [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

getNeighbors4P :: Point -> [Point]
getNeighbors4P (Point r c) = [Point (r - 1) c, Point r (c + 1), Point (r + 1) c, Point r (c - 1)]

get4DirsFromPoint :: Int -> Int -> (Int, Int) -> [[(Int, Int)]]
get4DirsFromPoint rMax cMax (r, c) = [up, down, left, right]
  where
    up = if c == 0 then [] else [(r, c') | c' <- [0 .. c - 1]]
    down = if c == cMax then [] else [(r, c') | c' <- [c + 1 .. cMax]]
    left = if r == 0 then [] else [(r', c) | r' <- [0 .. r - 1]]
    right = if r == rMax then [] else [(r', c) | r' <- [r + 1 .. rMax]]

getNeighbors8 :: (Int, Int) -> [(Int, Int)]
getNeighbors8 (r, c) = [(r', c') | r' <- [r - 1, r, r + 1], c' <- [c - 1, c, c + 1], (r', c') /= (r, c)]
