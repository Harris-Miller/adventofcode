module Day9 where

import Control.Monad.RWS
import Data.Function (on)
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (compare)

cSucc0 :: Int -> Int -> Int
cSucc0 max current = if n == max then 0 else n
  where
    n = succ current

cSucc1 :: Int -> Int -> Int
cSucc1 max current = if n > max then 1 else n
  where
    n = succ current

sPred :: Int -> Int -> Int
sPred max current = if n < 0 then max - 1 else n
  where
    n = pred current

back7 :: Int -> Int -> Int
back7 max current = last $ take 8 $ iterate (sPred max) current -- take 8 since we want index 7

insertAt :: Int -> Int -> [Int] -> [Int]
insertAt val _ [] = [val]
insertAt val 0 (x : xs) = x : val : xs
insertAt val idx (x : xs) = x : insertAt val (idx - 1) xs

removeAt :: Int -> [Int] -> (Int, [Int])
removeAt idx xs = go idx [] xs
  where
    go :: Int -> [Int] -> [Int] -> (Int, [Int])
    go 0 acc (x : xs) = (x, reverse acc <> xs)
    go idx acc (x : xs) = go (idx - 1) (x : acc) xs

-- placeNextMarble :: [Int] -> Int -> Int -> [Int]
-- placeNextMarble circle idx val = insertAt val idxToPlaceAt circle
--   where
--     idxToPlaceAt = cSucc0 (length circle) idx

somethingCompletelyDifferent :: [Int] -> RWS (Int, Int) [(Int, [Int])] (Int, Int, Int, Map Int Int) [Int]
somethingCompletelyDifferent circle = do
  (numPlayers, _) <- ask
  (player, idx, val, scores) <- get

  let idxToSteal = back7 (length circle) idx
  let (marbleVal, nextCircle) = removeAt idxToSteal circle
  let scoreToAdd = marbleVal + val
  tell [(player, nextCircle)]

  let newScores = M.insertWith (+) player scoreToAdd scores
  put (cSucc1 numPlayers player, idxToSteal, val + 1, newScores)
  return nextCircle

placeNextMarble :: [Int] -> RWS (Int, Int) [(Int, [Int])] (Int, Int, Int, Map Int Int) [Int]
placeNextMarble circle = do
  (numPlayers, _) <- ask
  (player, idx, val, scores) <- get

  let idxToPlaceAt = cSucc0 (length circle) idx
  let nextCircle = insertAt val idxToPlaceAt circle

  tell [(player, nextCircle)]

  put (cSucc1 numPlayers player, idxToPlaceAt + 1, val + 1, scores)
  return nextCircle

play :: [Int] -> RWS (Int, Int) [(Int, [Int])] (Int, Int, Int, Map Int Int) ()
play circle = do
  (numPlayers, lastMarble) <- ask
  (player, idx, val, scores) <- get

  nextCircle <- case val `mod` 23 of
    0 -> somethingCompletelyDifferent circle
    _ -> placeNextMarble circle

  (_, _, val, _) <- get
  if val > lastMarble then return () else play nextCircle

main' :: IO ()
main' = do
  -- 466 players; last marble is worth 71436 points
  let players = 466
  let lastMarble = 71436
  let scores = M.fromList $ map (,0) [1 .. 9]
  let (a, s, w) = runRWS (play [0]) (players, lastMarble) (1, 0, 1, scores)

  let (_, _, _, finalScores) = s

  let winner = maximumBy (compare `on` snd) $ M.toList finalScores

  -- mapM_ print w
  print winner
