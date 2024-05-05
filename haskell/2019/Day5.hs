module Day5 where

import Control.Monad.RWS
import Data.List.Split
import qualified Data.Map.Strict as M
import IntCode

main' :: IO ()
main' = do
  contents <- zip [0 ..] . map read . splitOn "," . head . lines <$> readFile "2019/inputs/Day5/input.txt" :: IO [(Int, Int)]
  let m = M.fromList contents
  let theM = processIntCode m
  -- part 1
  (a, s, w) <- runRWST theM () ([1], 0)
  print $ map snd $ M.toList a
  print w
  print s

  -- part 2
  (a, s, w) <- runRWST theM () ([5], 0)
  print $ map snd $ M.toList a
  print w
  print s
