module Day5 where

import Control.Lens
import Control.Monad.RWS.Strict
import Data.Map (Map)
import Data.Map qualified as M

process :: Map Int Int -> RWS () [Int] Int (Map Int Int)
process m = do
  i <- get
  if i >= M.size m
    then return m
    else do
      tell [i]
      let x = (M.!) m i
      put (i + x)
      process $ M.adjust (+ 1) i m

process2 :: Map Int Int -> RWST () [Int] Int IO (Map Int Int)
process2 m = do
  i <- get
  if i >= M.size m
    then return m
    else do
      tell [i]
      let x = (M.!) m i
      put (i + x)
      process2 $ M.adjust (if x >= 3 then subtract 1 else (+ 1)) i m

main' :: IO ()
main' = do
  contents <- M.fromList . zip [0 ..] . map read . lines <$> readFile "../inputs/2017/Day5/input.txt" :: IO (Map Int Int)
  -- part 1
  let (a, s, w) = runRWS (process contents) () 0
  print $ length w
  -- part 2
  (a, s, w) <- runRWST (process2 contents) () 0
  print $ length w
  return ()
