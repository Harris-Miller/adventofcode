module Day15 where

import Control.Arrow
import Data.Char (ord)
import Data.List
import Data.List.Index
import Data.List.Split
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple.Common

type Boxes = Map Int [(String, Int)]

process :: String -> Int
process = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (((acc + ord x) * 17) `rem` 256) xs

insertIntoBox :: Boxes -> String -> Boxes
insertIntoBox boxes s = M.insert boxNum box' boxes
  where
    (label, val) = second read $ toTuple $ splitOn "=" s
    boxNum = process label
    box = boxes ! boxNum
    i = findIndex ((== label) . fst) box
    box' = maybe (box <> [(label, val)]) (\i' -> setAt i' (label, val) box) i

removeFromBox :: Boxes -> String -> Boxes
removeFromBox boxes s = if isNothing i then boxes else M.insert boxNum box' boxes
  where
    label = init s
    boxNum = process label
    box = boxes ! boxNum
    i = findIndex ((== label) . fst) box
    box' = deleteAt (fromJust i) box

whatDo :: Boxes -> String -> Boxes
whatDo boxes s
  | '=' `elem` s = insertIntoBox boxes s
  | otherwise = removeFromBox boxes s

process2 :: [String] -> Boxes
process2 = go (M.fromList $ map (,[]) [0 .. 255])
  where
    go m [] = m
    go m (x : xs) = go (whatDo m x) xs

main' :: IO ()
main' = do
  contents <- splitOn "," . init <$> readFile "2023/inputs/Day15/input.txt"
  -- part 1
  print $ sum $ map process contents
  -- part 2
  let r = M.filter (not . null) $ process2 contents
  let r2 = sum $ M.elems $ M.mapWithKey (\k a -> sum $ zipWith (\i (_, v) -> (k + 1) * i * v) [1 ..] a) r
  print r2
  return ()
