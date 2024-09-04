module Day8 where

import Control.Arrow (Arrow (first, second))
import Data.Array.IArray
import Data.Bool
import Data.Grid.Array
import Data.List
import Data.List.Split
import Data.Tuple.Common

-- sample 7 wide, 3 tall
-- input  50 wide, 6 tall
width = 50 :: Int

height = 6 :: Int

drawRect :: String -> GridUA Bool -> GridUA Bool
drawRect inst grid = grid // [((c, r), True) | c <- [0 .. width - 1], r <- [0 .. height - 1]]
  where
    (width, height) = toTuple $ map read $ splitOn "x" $ drop 5 inst

rotateColumn :: String -> GridUA Bool -> GridUA Bool
rotateColumn inst grid = grid // zipWith (\from to -> (to, grid ! from)) pointFrom pointTo
  where
    (col, by) = tmap read $ first (drop 2) $ toTuple $ splitOn " by " $ drop 14 inst :: (Int, Int)
    pointFrom = map (col,) [0 .. height - 1]
    pointTo = map (\r -> (col, (r + by) `mod` height)) [0 .. height - 1]

rotateRow :: String -> GridUA Bool -> GridUA Bool
rotateRow inst grid = grid // zipWith (\from to -> (to, grid ! from)) pointFrom pointTo
  where
    (row, by) = tmap read $ first (drop 2) $ toTuple $ splitOn " by " $ drop 11 inst :: (Int, Int)
    pointFrom = map (,row) [0 .. width - 1]
    pointTo = map (\c -> ((c + by) `mod` width, row)) [0 .. width - 1]

processInst :: String -> GridUA Bool -> GridUA Bool
processInst inst grid
  | "rect" `isPrefixOf` inst = drawRect inst grid
  | "rotate column" `isPrefixOf` inst = rotateColumn inst grid
  | "rotate row" `isPrefixOf` inst = rotateRow inst grid

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2016/Day8/input.txt"
  print contents
  let grid = genArray ((0, 0), (width - 1, height - 1)) (const False) :: GridUA Bool
  let r = foldr processInst grid (reverse contents)
  print $ length $ filter id $ elems r
  mapM_ putStrLn $ gridToListString (bool '.' '#') r
