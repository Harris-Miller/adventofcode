module Day17 where

import Data.Point

parse :: String -> (Int, Int, Int)
parse s = (x, lowY, highY)
  where
    [(x, rest)] = (reads :: ReadS Int) $ drop 2 s
    [(lowY, rest2)] = (reads :: ReadS Int) $ drop 4 rest
    [(highY, _)] = (reads :: ReadS Int) $ drop 2 rest2

toPoints :: (Int, Int, Int) -> [Point]
toPoints (x, lowY, highY) = [Point x y | y <- [lowY .. highY]]

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2018/Day17/sample.txt"
  let minY = minimum $ map (\(_, lowY, _) -> lowY) contents
  let maxY = maximum $ map (\(_, _, highY) -> highY) contents
  let minX = minimum $ map (\(x, _, _) -> x) contents
  let maxX = maximum $ map (\(x, _, _) -> x) contents

  let asPoints = concatMap toPoints contents

  mapM_ print asPoints

  return ()
