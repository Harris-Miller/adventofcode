module Day10 where

import Data.Grid.HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List
import Data.Point

parse :: String -> (Point, Point)
parse s =
  let [(posX, r1)] = (reads :: ReadS Int) $ drop 10 s
      [(posY, r2)] = (reads :: ReadS Int) $ drop 1 r1
      [(velX, r3)] = (reads :: ReadS Int) $ drop 12 r2
      [(velY, _)] = (reads :: ReadS Int) $ drop 1 r3
   in (Point posX posY, Point velX velY)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2018/Day10/sample.txt"
  let (ps, vs) = unzip $ map parse contents
  let minX = minimum $ map px ps
  let maxX = maximum $ map px ps
  let minY = minimum $ map py ps
  let maxY = maximum $ map py ps

  let grid = gridHMToStr (Point minX minY) (Point maxX maxY) (const '.') (const '#') (HM.fromList $ map (,True) ps)
  -- mapM_ print $ lines grid

  return ()
