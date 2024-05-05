module Day6 where

import Data.Array.Unboxed
import Data.List
import Data.List.Split
import Data.Tuple.Common

data Operation = TurnOn | TurnOff | Toggle
  deriving (Show)

parsePoint :: String -> (Int, Int)
parsePoint = toTuple . map read . splitOn ","

parseOp :: String -> (Operation, (Int, Int))
parseOp s
  | "turn on" `isPrefixOf` s = (TurnOn, parsePoint $ drop 8 s)
  | "turn off" `isPrefixOf` s = (TurnOff, parsePoint $ drop 9 s)
  | "toggle" `isPrefixOf` s = (Toggle, parsePoint $ drop 7 s)
  | otherwise = error "Something went horribly wrong"

parse :: String -> (Operation, (Int, Int), (Int, Int))
parse s = (o, x, y)
  where
    [h, t] = splitOn " through " s
    (o, x) = parseOp h
    y = parsePoint t

expandPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
expandPoints (x1, y1) (x2, y2) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

-- part 1

opUpdateFn :: Operation -> (Bool -> Bool)
opUpdateFn TurnOn = const True
opUpdateFn TurnOff = const False
opUpdateFn Toggle = not

processOp :: UArray (Int, Int) Bool -> [(Operation, (Int, Int), (Int, Int))] -> UArray (Int, Int) Bool
processOp arr [] = arr
processOp arr ((op, p1, p2) : xs) = processOp (accum (\e f -> f e) arr list) xs
  where
    fn = opUpdateFn op
    list = map (,fn) $ expandPoints p1 p2

-- part 2

opUpdateFn2 :: Operation -> (Int -> Int)
opUpdateFn2 TurnOn = (+ 1)
opUpdateFn2 TurnOff = \x -> max 0 $ x - 1
opUpdateFn2 Toggle = (+ 2)

processOp2 :: UArray (Int, Int) Int -> [(Operation, (Int, Int), (Int, Int))] -> UArray (Int, Int) Int
processOp2 arr [] = arr
processOp2 arr ((op, p1, p2) : xs) = processOp2 (accum (\e f -> f e) arr list) xs
  where
    fn = opUpdateFn2 op
    list = map (,fn) $ expandPoints p1 p2

main' :: IO ()
main' = do
  ops <- map parse . lines <$> readFile "2015/inputs/Day6/input.txt"
  let points = expandPoints (0, 0) (999, 999)
  -- part 1
  let arr = array ((0, 0), (999, 999)) $ map (,False) points
  print $ (length . filter id . elems) $ processOp arr ops

  -- part 2
  let arr2 = array ((0, 0), (999, 999)) $ map (,0) points
  print $ (sum . elems) $ processOp2 arr2 ops
  return ()
