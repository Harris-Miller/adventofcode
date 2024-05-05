module Day13 where

import Control.Monad.Writer
import Data.Bifunctor
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Common

parse :: String -> (Int, Int)
parse = toTuple . map read . splitOn ": "

incrementFirewall :: (Int, Int) -> (Int, Int)
incrementFirewall (size, i) = (size, (i + 1) `mod` ((size - 1) * 2))

getPosition :: (Int, Int) -> Int
getPosition (size, i) = if i > maxI then maxI + (maxI - i) else i
  where
    maxI = size - 1

process :: Writer [Int] (M.Map Int (Int, Int)) -> Int -> Writer [Int] (M.Map Int (Int, Int))
process writer pos = do
  fws <- writer
  let fw = M.lookup pos fws
  let isCaught = maybe False ((== 0) . getPosition) fw
  tell [pos * (fst . fromJust) fw | isCaught]
  return $ M.map incrementFirewall fws

findNotCaught :: [Int] -> M.Map Int (Int, Int) -> Int
findNotCaught path fws = go fws 0
  where
    go fws' delay = if r == 0 then delay else go (M.map incrementFirewall fws') (delay + 1)
      where
        r = (sum . execWriter) $ foldl process (writer (fws', [])) path

day13a :: [(Int, Int)] -> Int
day13a input = sum [d * n | (d, n) <- input, d `mod` (2 * n - 2) == 0]

day13b :: [(Int, Int)] -> Int
day13b input = head [i | i <- [0 ..], not $ caught i]
  where
    caught i = or [(d + i) `mod` (2 * n - 2) == 0 | (d, n) <- input]

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2017/Day13/input.txt" :: IO [(Int, Int)]
  let fws = (M.fromList . map (second (,0))) contents :: M.Map Int (Int, Int)
  let lastFirewall = (fst . M.findMax) fws
  let path = [0 .. lastFirewall]
  print $ (sum . execWriter) $ foldl process (writer (fws, [])) path
  -- part 2
  print $ findNotCaught path fws
  -- other
  print $ day13a contents
  print $ day13b contents
