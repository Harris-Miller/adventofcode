module Day13 where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple.Select

showThing :: (String, String, Int) -> String
showThing (p1, p2, h) = p1 ++ " " ++ p2 ++ " " ++ show h

posOrNeg :: String -> Int -> Int
posOrNeg "lose" v = negate v
posOrNeg _ v = v

parse :: String -> (String, String, Int)
parse s = (good, bad, posOrNeg op $ read h)
  where
    [xs, bad] = splitOn " happiness units by sitting next to " s
    [good, xs'] = splitOn " would " xs
    [op, h] = words xs'

calcHappiness :: [(String, String, Int)] -> [String] -> Int
calcHappiness xs ps = go (findP (head ps) (last ps) + findP (last ps) (head ps)) ps
  where
    findP p1 p2 = sel3 . fromJust $ find (and . sequence [(== p1) . sel1, (== p2) . sel2]) xs
    go acc [p1, p2] = acc + findP p1 p2 + findP p2 p1
    go acc (p1 : p2 : ps') = go (acc + findP p1 p2 + findP p2 p1) (p2 : ps')

main' :: IO ()
main' = do
  contents <- map (parse . init) . lines <$> readFile "2015/inputs/Day13/input.txt"
  let people = nub $ map sel1 contents

  -- part 1
  let combos = permutations people
  let totals = map (calcHappiness contents) combos
  print $ maximum totals

  -- part 2
  let me = "Me"
  let plusMe = concatMap (\p -> [(me, p, 0), (p, me, 0)]) people
  let combos2 = permutations $ me : people
  let totals2 = map (calcHappiness (contents <> plusMe)) combos2
  print $ maximum totals2
