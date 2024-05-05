module Day14 where

import Control.Arrow
import Control.Monad.State
import Data.Array
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple.Select

showT :: (String, [Double]) -> String
showT (a, b) = a ++ " " ++ show b

showR :: (String, [Int], Int, Int) -> String
showR (name, _, pos, points) = name ++ " " ++ show pos ++ " " ++ show points

parse :: String -> [Int]
parse s = replicate sec speed <> replicate rest 0
  where
    [speed, sec, rest] = map fst . concatMap reads $ words s :: [Int]

tick' :: [[Int]] -> Array Int (Int, Int)
tick' reindeer = execState (go $ transpose reindeer) arr
  where
    arr = array (0, length reindeer - 1) $ map (,(0, 0)) [0 .. length reindeer - 1]
    go :: [[Int]] -> State (Array Int (Int, Int)) ()
    go [] = return ()
    go (x : xs) = do
      arr <- get
      -- first update arr with new current distances
      let x' = zip [0 ..] $ map (+) x
      let arr' = accum (flip first) arr x'
      -- next, determine current max
      let leadingDistance = maximum $ map fst $ elems arr'
      -- and find which indexes are current at that maximum
      let idx = map fst $ filter ((== leadingDistance) . fst . snd) $ assocs arr'
      -- and update the array one more time by increasing leader values
      let arr'' = accum (flip second) arr' $ map (,(+ 1)) idx
      put arr''
      go xs

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "2015/inputs/Day14/input.txt"
  let seconds = 2503

  -- part 1
  let results = map (sum . take seconds . cycle) contents
  print $ maximum results

  -- part 2
  let contents2 = map (take seconds . cycle) contents
  let results = tick' contents2
  let winner = maximum $ map snd $ elems results
  print winner
  -- -- mapM_ (print . showR) results
  -- let winner = maximumBy (compare `on` sel4) results
  -- print $ sel4 winner

  return ()
