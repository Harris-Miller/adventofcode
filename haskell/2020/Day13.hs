module Day13 where

import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text.Unsafe (iter)
import Data.Tuple.Common

parseBuses :: String -> [(Int, Int)]
parseBuses = map (second read) . filter ((/= "x") . snd) . zip [0 ..] . splitOn ","

waitTime :: Int -> Int -> Int
waitTime t b = b - (t `mod` b)

findFirstBus :: Int -> [Int] -> (Int, Int)
findFirstBus t bs = minimum (zip (map (waitTime t) bs) bs)

primesAndRemainders :: [(Int, Int)] -> [(Int, Int)]
primesAndRemainders = map (\p@(i, b) -> (b, uncurry waitTime p))

eEuclid :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
eEuclid (r, s, t) (r', s', t')
  | r' == 0 = (r, s, t)
  | otherwise =
      let q = r `div` r'
       in eEuclid (r', s', t') (r `mod` r', s - q * s', t - q * t')

bezout :: (Int, Int) -> (Int, Int)
bezout (a, b) = (s, t)
  where
    (_, s, t) = eEuclid (a, 1, 0) (b, 0, 1)

chineseRemainder :: ([Int], [Int]) -> Int
chineseRemainder (ps, rs) = x `mod` n
  where
    n = product ps
    ns = map (div n) ps
    ms = zipWith (curry (fst . bezout)) ns ps
    x = sum $ zipWith3 (\a b c -> a * b * c) rs ms ns

main' :: IO ()
main' = do
  (timestamp, positions) <- bimap read parseBuses . toTuple . lines <$> readFile "../inputs/2020/Day13/input.txt" :: IO (Int, [(Int, Int)])
  -- part 1
  let buses = map snd positions
  let (leaveTimeD, busId) = findFirstBus timestamp buses
  print $ leaveTimeD * busId

  -- part 2
  print $ chineseRemainder $ unzip $ primesAndRemainders positions
  return ()
