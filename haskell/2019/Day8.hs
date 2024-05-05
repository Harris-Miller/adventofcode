module Day8 where

import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Ord

processLayer :: [Int] -> [Int] -> [Int]
processLayer = zipWith (\b t -> if t == 2 then b else t)

process :: [[Int]] -> [Int]
process [b, t] = processLayer b t
process (b : t : xs) = process (processLayer b t : xs)

main' :: IO ()
main' = do
  contents <- map digitToInt . head . lines <$> readFile "../inputs/2019/Day8/input.txt" :: IO [Int]
  let rows = chunksOf (25 * 6) contents

  -- part 1
  let i = fst $ minimumBy (compare `on` snd) $ zip [0 ..] $ map (length . filter (== 0)) rows
  let row = rows !! i
  let [a, b, _] = map length $ (group . sortBy (comparing Down)) row
  print $ a * b

  -- part 2
  let singleLayer = process $ reverse rows
  let image = map unwords $ chunksOf 25 $ map (\x -> if x == 1 then "*" else " ") singleLayer
  mapM_ print image
