module Day17 where

import Control.Lens (Identity)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS.Strict
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List
import Data.Maybe
import Data.Point
import Prelude hiding (Left, Right)

data Dir = Left | Right | Down
  deriving (Show, Eq)

parse :: [Char] -> [Dir]
parse xs = intersperse Down (map go xs) <> [Down]
  where
    go '<' = Left
    go '>' = Right

type ShapePoints = HashSet Point

type Rocks = HashSet Point

data Shape = Flat | Plus | L | Straight | Block
  deriving (Show, Eq)

type RR = (Int, [Dir])

data SS = SS {shape :: Shape, pos :: Point, rocks :: Rocks, count :: Int, dirs :: [Dir]}
  deriving (Show)

type CavernT m a = RWST RR () SS m a

type Cavern a = CavernT Identity a

getNextShape :: Shape -> Shape
getNextShape Flat = Plus
getNextShape Plus = L
getNextShape L = Straight
getNextShape Straight = Block
getNextShape Block = Flat

currentHighRow :: Rocks -> Int
currentHighRow = HS.foldr (max . py) 0

shapeStartingPoint :: Shape -> Rocks -> Point
shapeStartingPoint shape grid
  | shape == Plus = Point 3 r
  | otherwise = Point 2 r
  where
    r = currentHighRow grid + 4

getShapePoints :: Shape -> Point -> ShapePoints
getShapePoints Flat (Point c r) = HS.fromList $ map (`Point` r) [c .. c + 3]
getShapePoints Plus (Point c r) = HS.fromList [Point c r, Point (c - 1) (r + 1), Point c (r + 1), Point (c + 1) (r + 1), Point c (r + 2)]
getShapePoints L (Point c r) = HS.fromList $ map (`Point` r) [c .. c + 2] <> map (Point (c + 2)) [r + 1 .. r + 2]
getShapePoints Straight (Point c r) = HS.fromList $ map (Point c) [r .. r + 3]
getShapePoints Block (Point c r) = HS.fromList [Point c r, Point (c + 1) r, Point c (r + 1), Point (c + 1) (r + 1)]

getShapeRBounds :: Shape -> Point -> (Int, Int)
getShapeRBounds Flat (Point c _) = (c, c + 3)
getShapeRBounds Plus (Point c _) = (c - 1, c + 1)
getShapeRBounds L (Point c _) = (c, c + 2)
getShapeRBounds Straight (Point c _) = (c, c)
getShapeRBounds Block (Point c _) = (c, c + 1)

moveShape :: Point -> ShapePoints -> Cavern ()
moveShape nextPos nextShapePoints = do
  SS {..} <- get
  let toRemove = getShapePoints shape pos
  let nextRocks = (rocks `HS.difference` toRemove) `HS.union` nextShapePoints
  modify (\ss -> ss {pos = nextPos, rocks = nextRocks})

repeatTilBottom :: CavernT IO ()
repeatTilBottom = do
  (_, reset) <- ask
  SS {..} <- get

  let (dir, remaining) = fromJust $ uncons dirs
  let nextPos = case dir of
        Left -> Point (px pos - 1) (py pos)
        Right -> Point (px pos + 1) (py pos)
        Down -> Point (px pos) (py pos - 1)

  -- liftIO $ do
  --   printRocks rocks
  --   putStrLn ""

  let nextDirs = if null remaining then reset else remaining
  modify (\ss -> ss {dirs = nextDirs})

  let rocksWithout = rocks `HS.difference` getShapePoints shape pos
  let nextShapePoints = getShapePoints shape nextPos
  let (lb, rb) = getShapeRBounds shape nextPos
  let canMove = py nextPos > -1 && lb > -1 && rb < 7 && nextShapePoints == nextShapePoints `HS.difference` rocksWithout

  let go
        | dir == Down && not canMove = return ()
        | not canMove = repeatTilBottom
        | otherwise = modify (\ss -> ss {pos = nextPos, rocks = rocksWithout `HS.union` nextShapePoints}) >> repeatTilBottom
  go

insertNewShapeOrEnd :: CavernT IO ()
insertNewShapeOrEnd = do
  repeatTilBottom

  (maxCount, _) <- ask
  SS {..} <- get

  -- liftIO $ do
  --   printRocks rocks
  --   putStrLn ""

  let nextCount = count + 1
  let nextShape = getNextShape shape
  let nextPos = shapeStartingPoint nextShape rocks
  let nextRocks = rocks `HS.union` getShapePoints nextShape nextPos
  if nextCount == maxCount then return () else modify (\ss -> ss {count = nextCount, shape = nextShape, pos = nextPos, rocks = nextRocks}) >> insertNewShapeOrEnd

printRocks :: Rocks -> IO ()
printRocks rocks = do
  let top = currentHighRow rocks
  let ls = map (\r -> map (\c -> if HS.member (Point c r) rocks then '#' else '.') [0 .. 6]) [top, top - 1 .. 0]
  mapM_ print ls
  print "-------"

main' :: IO ()
main' = do
  contents <- head . lines <$> readFile "../inputs/2022/Day17/input.txt"
  let dirs = parse contents

  let emptyRocks = HS.empty :: Rocks
  let startingPos = Point 2 3 -- need to do special since there are no rocks to start
  let startingRocks = emptyRocks `HS.union` getShapePoints Flat startingPos
  let initState = SS {shape = Flat, pos = startingPos, rocks = startingRocks, count = 1, dirs = dirs}
  -- printRocks (rocks initState)
  -- putStrLn ""

  -- let maxCount = 10
  let maxCount = 2023

  (_, ss, _) <- runRWST insertNewShapeOrEnd (maxCount, dirs) initState
  -- printRocks (rocks ss)
  print $ currentHighRow (rocks ss) + 1 -- + 1 because rock tower starts at zero
  return ()
