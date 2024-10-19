module Day6 where

import Control.Monad.RWS
import Data.Bifunctor
import Data.Grid.HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List
import Data.Maybe
import Data.Ord
import Data.Point

parse :: String -> Point
parse s = Point x y
  where
    [(x, s')] = (reads :: ReadS Int) s
    [(y, _)] = (reads :: ReadS Int) $ drop 2 s'

-- for each elem in xs, keep if another elem in xs exists in all for quadrants on the XY plane
-- in other words, a point that is (< x, <y), (> x, <y), (> x, >y), (< x, <y)
findEncapsulations :: [Point] -> [Point]
findEncapsulations xs = filter (\x -> any (pointInQ1 x) xs && any (pointInQ2 x) xs && any (pointInQ3 x) xs && any (pointInQ4 x) xs) xs
  where
    pointInQ1 p other = px p < px other && py p < py other
    pointInQ2 p other = px p < px other && py p > py other
    pointInQ3 p other = px p > px other && py p > py other
    pointInQ4 p other = px p > px other && py p < py other

process :: [(Point, [Point])] -> RWST [Point] () (HashMap Point [Point]) IO ()
process newPoints = do
  let allNewPoints = concatMap snd newPoints
  -- these are new points made by more than point point-of-origin, thus not closer to any
  let pointsToRemove = (HS.fromList . map head . filter ((/= 1) . length) . group . sort) allNewPoints
  allExistingPoints <- gets (HS.fromList . concat . HM.elems)
  let newPoints' = map (second (filter (not . (`HS.member` allExistingPoints)) . filter (not . (`HS.member` pointsToRemove)))) newPoints
  modify (HM.unionWith (<>) (HM.fromList newPoints'))
  rs <- ask
  if all (null . fromJust . (`lookup` newPoints')) rs then return () else process (map (second (nub . concatMap getNeighbors4P)) newPoints')

part1 :: [Point] -> [Point] -> IO (HashMap Point [Point])
part1 rs ps =
  let input = map (\p -> (p, getNeighbors4P p)) ps
      ss = HM.fromList $ map (\p -> (p, [p])) ps
   in fst <$> execRWST (process input) rs ss

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2018/Day6/sample.txt"
  let es = findEncapsulations contents
  r1 <- part1 es contents
  let r1' = maximumBy (comparing (length . snd)) $ HM.toList $ HM.filterWithKey (\k _ -> k `elem` es) r1
  print r1'
  print "Day 6"
