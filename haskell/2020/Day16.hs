module Day16 where

import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe
import Data.Range
import Data.Tuple.Common

parseRules :: String -> (String, ((Int, Int), (Int, Int)))
parseRules s = (name, what)
  where
    (name, rest) = toTuple $ splitOn ": " s
    what = tmap (toTuple . map read . splitOn "-") $ (toTuple . splitOn " or ") rest :: ((Int, Int), (Int, Int))

validForAllFields :: Int -> [(Int, Int)] -> Bool
validForAllFields val ranges = inRanges ranges' val
  where
    ranges' = map (uncurry (+=+)) ranges

maybeField :: [Int] -> (String, ((Int, Int), (Int, Int))) -> Maybe String
maybeField xs (name, (l, h)) = if all (inRanges ranges) xs then Just name else Nothing
  where
    ranges = [uncurry (+=+) l, uncurry (+=+) h]

doTheThing :: [[String]] -> [[String]]
doTheThing xs = if all ((== 1) . length) xs then xs else doTheThing (map (\x -> if length x == 1 then x else x \\ singles) xs)
  where
    singles = concat $ filter ((== 1) . length) xs

main' :: IO ()
main' = do
  content <- splitOn "\n\n" <$> readFile "../inputs/2020/Day16/input.txt"

  let rules = (map parseRules . lines) (head content)
  let yourTickets = (map read . splitOn "," . last . lines) (content !! 1) :: [Int]
  let nearbyTickets = (map (map read . splitOn ",") . drop 1 . lines) (content !! 2) :: [[Int]]

  let allRanges = concatMap (toList . snd) rules

  let r = mapMaybe (find (not . (`validForAllFields` allRanges))) nearbyTickets
  print $ sum r

  -- part 2
  let nearbyTickets' = filter (all (`validForAllFields` allRanges)) nearbyTickets
  let byColumn = transpose nearbyTickets'

  let r = map (\r -> mapMaybe (maybeField r) rules) byColumn

  let columns = zip [0 ..] $ map head $ doTheThing r

  let startsWithDeparture = map fst $ filter (isPrefixOf "departure" . snd) columns

  let final = foldr (\i acc -> acc * (yourTickets !! i)) 1 startsWithDeparture

  print final

  return ()
