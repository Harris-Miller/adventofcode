module Day4 where

import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tuple.Common

parseCard :: String -> (Int, ([Int], [Int]))
parseCard = bimap (read . drop 5) (tmap (map read . filter (not . null) . splitOn " ") . toTuple . splitOn " | ") . toTuple . splitOn ": "

determineWinners :: [([Int], [Int])] -> [Int]
determineWinners = map ((2 ^) . (1 `subtract`)) . filter (/= 0) . map (length . uncurry intersect)

processCards :: Map Int ([Int], [Int]) -> [Int]
processCards m = go $ M.keys m
  where
    cardsWon = M.mapWithKey (\k v -> [k + 1 .. k + v]) $ M.filter (/= 0) $ M.map (length . uncurry intersect) m
    go :: [Int] -> [Int]
    go [] = []
    go xs = xs <> go (concat $ mapMaybe (`M.lookup` cardsWon) xs)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2023/inputs/Day4/input.txt"
  let cards = M.fromList $ map parseCard contents
  -- part 1
  let winners = determineWinners $ M.elems cards
  print $ sum winners
  -- part 2
  let totalCards = processCards cards
  print $ length totalCards
  return ()
