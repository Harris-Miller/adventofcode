module Day17 where

import Control.Monad.RWS
import Data.Array
import Data.Grid.Array
import Data.List
import Data.Maybe

data Square = Rock | Air | Water
  deriving (Show, Eq)

type GridMonad a = RWS Int () (Array (Int, Int) Square) a

squareToChar :: Square -> Char
squareToChar Rock = '#'
squareToChar Air = '.'
squareToChar Water = '~'

parseVerticalLine :: String -> [(Int, Int)]
parseVerticalLine s = [(x, y) | x <- [lowX .. highX]]
  where
    [(y, rest)] = (reads :: ReadS Int) $ drop 2 s
    [(lowX, rest2)] = (reads :: ReadS Int) $ drop 4 rest
    [(highX, _)] = (reads :: ReadS Int) $ drop 2 rest2

parseHorizontalLine :: String -> [(Int, Int)]
parseHorizontalLine s = [(x, y) | y <- [lowY .. highY]]
  where
    [(x, rest)] = (reads :: ReadS Int) $ drop 2 s
    [(lowY, rest2)] = (reads :: ReadS Int) $ drop 4 rest
    [(highY, _)] = (reads :: ReadS Int) $ drop 2 rest2

parse :: String -> [(Int, Int)]
parse s@(c : _)
  | c == 'x' = parseHorizontalLine s
  | c == 'y' = parseVerticalLine s

springSource = (500, 0) :: (Int, Int)

upOne :: (Int, Int) -> (Int, Int)
upOne (x, y) = (x, y - 1)

rightOne :: (Int, Int) -> (Int, Int)
rightOne (x, y) = (x + 1, y)

leftOne :: (Int, Int) -> (Int, Int)
leftOne (x, y) = (x - 1, y)

downOne :: (Int, Int) -> (Int, Int)
downOne (x, y) = (x, y + 1)

findFirstOccupiedSpaceBelow :: GridMonad (Maybe (Int, Int))
findFirstOccupiedSpaceBelow = do
  grid <- get
  let maybePoint = find ((/= Air) . (grid !)) [(500, y) | y <- [0 ..]]
  return $ maybePoint >>= \p -> if p == springSource then Nothing else Just p

downOr :: ((Int, Int) -> GridMonad ()) -> (Int, Int) -> GridMonad ()
downOr fn p = do
  modify (// [(p, Water)])
  grid <- get
  let p' = downOne p
  -- if grid ! p' == Air then stopAtEndOrContinue p' else fn p'
  if grid ! p' == Air then return () else fn p

handleLeft :: (Int, Int) -> GridMonad ()
handleLeft p = do
  grid <- get
  let p' = leftOne p
  let v = grid ! p'
  case v of
    Air -> downOr handleLeft p'
    _ -> return ()

handleRight :: (Int, Int) -> GridMonad ()
handleRight p = do
  grid <- get
  let p' = rightOne p
  let v = grid ! p'
  case v of
    Air -> downOr handleRight p'
    _ -> return ()

handleDown :: (Int, Int) -> GridMonad ()
handleDown p = do
  grid <- get
  let p' = downOne p
  let v = grid ! p'
  case v of
    Air -> stopAtEndOrContinue p'
    _ -> spanWater p'

stopAtEndOrContinue :: (Int, Int) -> GridMonad ()
stopAtEndOrContinue p@(_, y) = do
  modify (// [(p, Water)])
  maxY <- ask
  state <- get
  if maxY == y then return () else handleDown p

spanWater :: (Int, Int) -> GridMonad ()
spanWater p = do
  modify (// [(p, Water)])
  handleLeft p >> handleRight p

fillWithWater :: GridMonad ()
fillWithWater = do
  maybeSpace <- findFirstOccupiedSpaceBelow
  case maybeSpace of
    Just space -> spanWater (upOne space) >> fillWithWater
    Nothing -> return ()

main' :: IO ()
main' = do
  contents <- concatMap parse . lines <$> readFile "../inputs/2018/Day17/sample.txt"
  let minX = minimum (map fst contents) - 1
  let maxX = maximum (map fst contents) + 1

  let minY = 0
  let maxY = maximum (map snd contents)

  let grid = array ((minX, minY), (maxX, maxY)) [((x, y), Air) | x <- [minX .. maxX], y <- [minY .. maxY]] // map (,Rock) contents

  let (_, result, _) = runRWS fillWithWater maxY grid
  mapM_ print $ gridToListString squareToChar result

  return ()
