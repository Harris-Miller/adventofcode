module Day12 where

import Control.Monad.RWS
import Control.Monad.State
import Data.Bifunctor
import Data.List
import Data.Point
import Data.Tuple.Select
import Prelude hiding (Left, Right)

-- Part 1

data Cardinal = North | East | South | West
  deriving (Show, Eq, Enum)

cardinalToChar :: Cardinal -> Char
cardinalToChar North = 'N'
cardinalToChar East = 'E'
cardinalToChar South = 'S'
cardinalToChar West = 'W'

cycleCardinal :: Cardinal -> Cardinal
cycleCardinal West = North
cycleCardinal c = succ c

turn :: Char -> Int -> State Cardinal ()
turn 'R' 90 = modify cycleCardinal
turn 'R' 180 = modify (cycleCardinal . cycleCardinal)
turn 'R' 270 = modify (cycleCardinal . cycleCardinal . cycleCardinal)
turn 'L' 90 = turn 'R' 270
turn 'L' 180 = turn 'R' 180
turn 'L' 270 = turn 'R' 90

strafe :: Char -> Int -> Point -> Point
strafe 'N' n p = p <> Point 0 n
strafe 'S' n p = p <> Point 0 (-n)
strafe 'E' n p = p <> Point n 0
strafe 'W' n p = p <> Point (-n) 0

forward :: Int -> Point -> State Cardinal Point
forward n pos = do
  d <- get
  pure (strafe (cardinalToChar d) n pos)

parseAction :: String -> Point -> State Cardinal Point
parseAction [] _ = error "parseAction failed"
parseAction [_] _ = error "parseAction failed"
parseAction (a : n) pos
  | a `elem` ("NESW" :: String) = pure (strafe a n' pos)
  | a `elem` ("LR" :: String) = turn a n' >> pure pos
  | a == 'F' = forward n' pos
  where
    n' = read n :: Int

-- Part 2

type Ship2 a = RWS () [String] Point a

pad4 :: String -> String
pad4 s = s <> mconcat (replicate padBy " ")
  where
    l = length s
    padBy = if l < 4 then 4 - l else 0

rotate90 :: Point -> Point
rotate90 (Point a b) = Point (-b) a

rotate :: Char -> Int -> Point -> Point
rotate 'L' 90 = rotate90
rotate 'L' 180 = rotate90 . rotate90
rotate 'L' 270 = rotate90 . rotate90 . rotate90
rotate 'R' 90 = rotate 'L' 270
rotate 'R' 180 = rotate 'L' 180
rotate 'R' 270 = rotate 'L' 90

tellNewWaypoint :: Char -> Int -> Ship2 ()
tellNewWaypoint c n = do
  wp <- get
  tell [pad4 (c : show n) ++ " :: New Waypoint " ++ show wp]
  return ()

turnWaypoint :: Char -> Int -> Ship2 ()
turnWaypoint c n = do
  modify (rotate c n)
  tellNewWaypoint c n

strafeWaypoint :: Char -> Int -> Ship2 ()
strafeWaypoint c n = do
  modify (strafe c n)
  tellNewWaypoint c n

forwardWaypoint :: Int -> Point -> Ship2 Point
forwardWaypoint n pos = do
  wp <- get
  let pos' = mconcat (pos : replicate n wp)
  tell [pad4 ("F" ++ show n) ++ " :: New Position " ++ show pos']
  return pos'

parseAction2 :: String -> Point -> Ship2 Point
parseAction2 [] _ = error "parseAction2 failed"
parseAction2 [_] _ = error "parseAction2 failed"
parseAction2 (a : n) pos
  | a `elem` ("NESW" :: String) = strafeWaypoint a n' >> pure pos
  | a `elem` ("LR" :: String) = turnWaypoint a n' >> pure pos
  | a == 'F' = forwardWaypoint n' pos
  where
    n' = read n :: Int

process :: [String] -> Point -> State Cardinal Point
process [] pos = pure pos
process (x : xs) pos = parseAction x pos >>= process xs

process2 :: [String] -> Point -> Ship2 Point
process2 [] pos = pure pos
process2 (x : xs) pos = parseAction2 x pos >>= process2 xs

main' :: IO ()
main' = do
  content <- lines <$> readFile "../inputs/2020/Day12/input.txt"
  -- part 1
  let end = runState (process content (Point 0 0)) East
  print $ (\(Point x y) -> x + y) $ abs (fst end)
  -- part 2
  putStrLn "Starting at (0,0) with waypoint at (0, 10)"
  let end2 = runRWS (process2 content (Point 0 0)) () (Point 10 1)
  -- mapM_ putStrLn (sel3 end2)
  print $ (\(Point x y) -> x + y) $ abs (sel1 end2)
