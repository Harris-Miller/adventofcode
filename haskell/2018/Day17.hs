module Day17 where

import Data.Array
import Data.Grid.Array

data Square = Rock | Air | Water
  deriving (Show, Eq)

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

main' :: IO ()
main' = do
  contents <- concatMap parse . lines <$> readFile "../inputs/2018/Day17/sample.txt"
  let minX = minimum (map fst contents) - 1
  let minY = minimum (map snd contents) - 1

  let maxX = maximum (map fst contents) + 1
  let maxY = maximum (map snd contents) + 1

  print (minX, minY)
  print (maxX, maxY)

  let grid = array ((minX, minY), (maxX, maxY)) [((x, y), Air) | x <- [minX .. maxX], y <- [minY .. maxY]] // map (,Rock) contents

  mapM_ print $ gridToListString squareToChar grid

  return ()
