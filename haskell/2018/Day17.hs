module Day17 where

import Control.Monad
import Control.Monad.RWS
import Data.Array
import Data.Grid.Array
import Data.List
import Data.Maybe

data Square = Rock | Air | Water
  deriving (Show, Eq)

type GridMonad a = RWS ((Int, Int), (Int, Int)) () (Array (Int, Int) Square) a

type GridMonadTIO a = RWST ((Int, Int), (Int, Int)) () (Array (Int, Int) Square) IO a

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

handleSpanLeft :: (Int, Int) -> GridMonad ()
handleSpanLeft p@(x, y) = do
  ((minX, _), _) <- ask
  grid <- get
  let openBelow = find ((== Air) . (grid !)) $ reverse [(x, y + 1) | x <- [minX .. x - 1]]
  let foundRock = find ((/= Air) . (grid !)) $ reverse [(x, y) | x <- [minX .. x - 1]]
  if isJust openBelow
    then let Just (x2, _) = openBelow in modify (// [((x', y), Water) | x' <- [x2 .. x - 1]]) -- >> handleSpanDown ((downOne . fromJust) openBelow)
    else
      when (isJust foundRock) $ let Just (x3, _) = foundRock in void (modify (// [((x', y), Water) | x' <- [x3 .. x - 1]]))

handleSpanRight :: (Int, Int) -> GridMonad ()
handleSpanRight p@(x, y) = do
  (_, (maxX, _)) <- ask
  grid <- get
  let openBelow = find ((== Air) . (grid !)) $ reverse [(x, y + 1) | x <- [x + 1 .. maxX]]
  let foundRock = find ((/= Air) . (grid !)) $ reverse [(x, y) | x <- [x + 1 .. maxX]]
  if isJust openBelow
    then modify (// [((x', y), Water) | x' <- [x + 1 .. (fst . fromJust) openBelow]]) -- >> handleSpanDown ((downOne . fromJust) openBelow)
    else
      when (isJust foundRock) $ void (modify (// [((x', y), Water) | x' <- [x + 1 .. (fst . fromJust) foundRock]]))

handleSpanDown :: (Int, Int) -> GridMonad ()
handleSpanDown p@(x, y) = do
  (_, (_, maxY)) <- ask
  maybeRock <- findFirstOccupiedSpaceBelow p
  if isJust maybeRock
    then modify (// [((x, y'), Water) | y' <- [y .. (snd . fromJust) maybeRock]]) >> spanWater (fromJust maybeRock)
    else void (modify (// [((x, y'), Water) | y' <- [y .. maxY]]))

findFirstOccupiedSpaceBelow :: (Int, Int) -> GridMonad (Maybe (Int, Int))
findFirstOccupiedSpaceBelow p@(x, y) = do
  (_, (_, maxY)) <- ask
  grid <- get
  let maybePoint = find ((/= Air) . (grid !)) [(x, y') | y' <- [y .. maxY]]
  return $ maybePoint >>= \p -> if p == springSource then Nothing else Just p

spanWater :: (Int, Int) -> GridMonad ()
spanWater p = do
  handleSpanLeft p -- >> handleSpanRight p

fillWithWater :: GridMonad ()
fillWithWater = do
  maybeSpace <- findFirstOccupiedSpaceBelow springSource
  case maybeSpace of
    Just space -> spanWater (upOne space) -- >> fillWithWater
    Nothing -> return ()

main' :: IO ()
main' = do
  contents <- concatMap parse . lines <$> readFile "../inputs/2018/Day17/sample.txt"
  let minX = minimum (map fst contents) - 1
  let maxX = maximum (map fst contents) + 1

  let minY = 0
  let maxY = maximum (map snd contents)

  let bounds = ((minX, minY), (maxX, maxY))

  let grid = array bounds [((x, y), Air) | x <- [minX .. maxX], y <- [minY .. maxY]] // map (,Rock) contents
  -- mapM_ print $ gridToListString squareToChar grid

  let (_, result, _) = runRWS fillWithWater bounds grid
  mapM_ print $ gridToListString squareToChar result

  putStrLn ""
