module Day14 where

import Control.Arrow
import Data.Grid.Map
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as M

moveRocks ::
  (Int, Int) -> -- maxes
  ((Int, Int) -> Bool) -> -- atEnd
  ((Int, Int) -> Bool) -> -- atBottom
  ((Int, Int) -> (Int, Int)) -> -- moveBack
  ((Int, Int) -> (Int, Int)) -> -- moveForward
  (Int, Int) -> -- start
  GridM Char -> -- grid
  GridM Char
moveRocks (maxR, maxC) atEnd atBottom moveBack moveForward start grid = go start grid
  where
    go :: (Int, Int) -> GridM Char -> GridM Char
    go (r, c) grid
      | atEnd (r, c) = grid
      | atBottom (r, c) = go (moveForward (r, c)) grid
      | otherwise = if val == 'O' && prev == '.' then go (moveBack (r, c)) swapEm else go (moveForward (r, c)) grid
      where
        val = grid ! (r, c)
        prev = grid ! moveBack (r, c)
        swapEm = M.insert (moveBack (r, c)) val $ M.insert (r, c) prev grid

moveWest :: (Int, Int) -> GridM Char -> GridM Char
moveWest (maxR, maxC) = moveRocks (maxR, maxC) atEnd atBottom moveBack moveForward (0, 0)
  where
    atEnd (r, _) = r > maxR
    atBottom (_, c) = c == 0
    moveBack (r, c) = (r, c - 1)
    moveForward (r, c) = if c + 1 > maxC then (r + 1, 0) else (r, c + 1)

moveNorth :: (Int, Int) -> GridM Char -> GridM Char
moveNorth (maxR, maxC) = moveRocks (maxR, maxC) atEnd atBottom moveBack moveForward (0, 0)
  where
    atEnd (_, c) = c > maxC
    atBottom (r, _) = r == 0
    moveBack (r, c) = (r - 1, c)
    moveForward (r, c) = if r + 1 > maxR then (0, c + 1) else (r + 1, c)

moveEast :: (Int, Int) -> GridM Char -> GridM Char
moveEast (maxR, maxC) = moveRocks (maxR, maxC) atEnd atBottom moveBack moveForward (maxR, maxC)
  where
    atEnd (r, _) = r < 0
    atBottom (_, c) = c == maxC
    moveBack (r, c) = (r, c + 1)
    moveForward (r, c) = if c - 1 < 0 then (r - 1, maxC) else (r, c - 1)

moveSouth :: (Int, Int) -> GridM Char -> GridM Char
moveSouth (maxR, maxC) = moveRocks (maxR, maxC) atEnd atBottom moveBack moveForward (maxR, maxC)
  where
    atEnd (_, c) = c < 0
    atBottom (r, _) = r == maxR
    moveBack (r, c) = (r + 1, c)
    moveForward (r, c) = if r - 1 < 0 then (maxR, c - 1) else (r - 1, c)

nwse :: (Int, Int) -> GridM Char -> GridM Char
nwse maxes = moveEast maxes . moveSouth maxes . moveWest maxes . moveNorth maxes

doXTimes :: (Int, Int) -> Int -> GridM Char -> GridM Char
doXTimes maxes 0 grid = grid
doXTimes maxes i grid = if next == grid then grid else doXTimes maxes (i - 1) next
  where
    next = nwse maxes grid

main' :: IO ()
main' = do
  contents <- readFile "2023/inputs/Day14/sample.txt"
  let grid = parseGridAsIs contents
  let (maxR, maxC) = fst $ M.findMax grid

  -- mapM_ putStrLn $ moveRocksSouth $ lines contents
  -- putStrLn ""
  -- mapM_ putStrLn $ lines $ gridCharToStr $ moveSouth maxes grid

  -- part 1
  let r = moveNorth (maxR, maxC) grid
  print $ sum $ map ((maxR + 1 -) . fst . fst) $ M.toList $ M.filter (== 'O') r
  -- print $ sum $ map (\(i, s) -> i * length (filter (== 'O') s)) ir

  -- part 2
  let r2 = doXTimes (maxR, maxC) 1000000000 grid
  print $ sum $ map ((maxR + 1 -) . fst . fst) $ M.toList $ M.filter (== 'O') r2
  return ()
