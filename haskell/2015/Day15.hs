module Day15 where

import Data.List
import Data.Tuple.Select

-- parse

parse :: String -> (String, [Int])
parse s =
  let [cap, dur, flav, tex, cal] = map fst . concatMap reads $ words s :: [Int]
      name = (init . head . words) s
   in (name, [cap, dur, flav, tex, cal])

-- util

combos2 :: [[Int]]
combos2 = [[a, b] | a <- [1 .. 99], b <- [1 .. 99], 100 == sum [a, b]]

combos4 :: [[Int]]
combos4 = [[a, b, c, d] | a <- [1 .. 97], b <- [1 .. 97], c <- [1 .. 97], d <- [1 .. 97], 100 == sum [a, b, c, d]]

-- funcs

process :: [Int] -> [(String, [Int])] -> (Int, Int)
process amounts ingredients = (score, calories)
  where
    ingredients' = map sel2 ingredients
    score = product $ map ((max 0 . sum . zipWith (*) amounts) . (\i -> map (!! i) ingredients')) [0 .. 3]
    calories = sum $ zipWith (*) amounts (map (!! 4) ingredients')

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "2015/inputs/Day15/input.txt"
  mapM_ print contents
  -- part 1
  print $ maximum $ map (fst . flip process contents) combos4
  -- part 2
  print $ maximum $ map fst $ filter ((== 500) . snd) $ map (`process` contents) combos4
