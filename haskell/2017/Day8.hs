module Day8 where

import Control.Monad.State
import Data.List
import Data.Map qualified as M
import Data.Maybe

data Operation = Operation
  { name :: String,
    op :: Int -> Int -> Int,
    by :: Int,
    other :: String,
    comparator :: Int -> Int -> Bool,
    to :: Int
  }

parseOp :: String -> Int -> Int -> Int
parseOp "inc" = (+)
parseOp "dec" = subtract

parseComparator :: String -> Int -> Int -> Bool
parseComparator ">" = (>)
parseComparator "<" = (<)
parseComparator "<=" = (<=)
parseComparator ">=" = (>=)
parseComparator "==" = (==)
parseComparator "!=" = (/=)

parse :: String -> Operation
parse s = Operation {name = name', op = parseOp op', by = read by', other = other', comparator = parseComparator comparator', to = read to'}
  where
    [name', op', by', _, other', comparator', to'] = words s

process :: [Operation] -> M.Map String Int -> Operation -> M.Map String Int
process xs rs x = if should then M.insertWith (op x) (name x) (by x) rs else rs
  where
    other' = fromJust $ find (\o -> name o == other x) xs
    value = fromJust $ M.lookup (name other') rs
    should = comparator x value (to x)

maximumMap :: M.Map String Int -> Int
maximumMap = maximum . map snd . M.toList

processM :: [Operation] -> M.Map String Int -> [Operation] -> State Int (M.Map String Int)
processM xs rs [] = do
  return rs
processM xs rs (x : rest) = do
  let rs' = process xs rs x
  modify (max (maximumMap rs'))
  processM xs rs' rest

main' :: IO ()
main' = do
  xs <- map parse . lines <$> readFile "../inputs/2017/Day8/input.txt"
  let rs = M.fromList $ map (\x -> (name x, 0)) xs :: M.Map String Int
  let r = foldl (process xs) rs xs
  let largest = maximum $ map snd $ M.toList r
  print largest
  let (_, largestAllTime) = runState (processM xs rs xs) 0
  print largestAllTime
