module Day5 where

import Data.List
import Data.Maybe

powers = reverse [0 .. 6]

parseSeat :: (String, String) -> (Int, Int)
parseSeat (rowCode, seatCode) = (r, s)
  where
    r = fst $ foldl (\(l, h) (p, x) -> if x == 'F' then (l, h - 2 ^ p) else (l + 2 ^ p, h)) (0, 127) (zip (reverse [0 .. 6]) rowCode)
    s = fst $ foldl (\(l, h) (p, x) -> if x == 'L' then (l, h - 2 ^ p) else (l + 2 ^ p, h)) (0, 7) (zip (reverse [0 .. 2]) seatCode)

main' :: IO ()
main' = do
  content <- map (splitAt 7) . lines <$> readFile "../inputs/2020/Day5/input.txt"
  -- Part 1
  let r1 = map ((\(r, s) -> r * 8 + s) . parseSeat) content
  print $ maximum r1
  -- Part 2
  let ix = zip [0 ..] (sort r1)
  let r2 = fromJust $ find (\(i, x) -> x - snd (ix !! (i - 1)) /= 1) $ drop 1 ix
  print $ snd r2 - 1
