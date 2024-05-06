module Day12 where

import Algorithm.Search
import Data.Bifunctor qualified as BiF
import Data.Function
import Data.List
import Data.Map qualified as M
import Data.Maybe

type Point = (Int, Int)

heightRef :: M.Map Char Int
heightRef = M.fromList $ zip ['a' .. 'z'] [1 ..] <> [('S', 1), ('E', 26)]

getHeight :: Char -> Int
getHeight = fromJust . flip M.lookup heightRef

lookupPoint :: Point -> M.Map Point a -> a
lookupPoint p coords = fromJust $ M.lookup p coords

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

calcDist :: M.Map Point Int -> Point -> Point -> Int
calcDist list pt1 pt2 = lookupPoint pt2 list - lookupPoint pt1 list

-- creators

createVertices :: (Int, Int) -> M.Map Point Int -> Point -> [Point]
createVertices (xMax, yMax) coords point@(x, y) = filtered
  where
    neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    isInBounds nMax n = n >= 0 && n <= nMax
    canMoveTo pt1 pt2 = (< 2) $ calcDist coords pt1 pt2
    filtered = filter (\point'@(x', y') -> isInBounds xMax x' && isInBounds yMax y' && canMoveTo point point') neighbors

createGrid :: [String] -> [(Point, Char)]
createGrid = concatMap sepLine . zipWithIndex
  where
    sepLine (i, line) = map (\(j, v) -> ((i, j), v)) $ zipWithIndex line

toHeightIntMap :: [(Point, Char)] -> M.Map Point Int
toHeightIntMap = M.fromList . map (BiF.second getHeight)

-- aStar funcs

dist :: Point -> Point -> Int
dist (xE, yE) (x, y) = abs (xE - x) + abs (yE - y)

getCost :: Point -> Point -> Int
getCost _ _ = 1 :: Int

-- main

main' :: IO ()
main' = do
  content <- lines <$> readFile "../inputs/2022/Day12/input.txt"
  let maxes = (length content - 1, length (head content) - 1)
  let grid = createGrid content

  let start1 = fst $ fromJust $ find (\(_, v) -> v == 'S') grid
  let end1 = fst $ fromJust $ find (\(_, v) -> v == 'E') grid

  let grid' = toHeightIntMap grid

  -- part 1
  let result = aStar (createVertices maxes grid') (\_ _ -> 1) (dist end1) (== end1) start1
  print $ length . snd <$> result

  -- part 2
  let starts2 = start1 : map fst (filter (\(_, v) -> v == 'a') grid)
  let results2 = mapMaybe (aStar (createVertices maxes grid') (\_ _ -> 1) (dist end1) (== end1)) starts2
  print $ minimum $ map (length . snd) results2
