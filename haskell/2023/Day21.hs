module Day21 where

import Data.Grid.Map
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

enclose :: (Int, Int) -> (Int, Int) -> Set (Int, Int)
enclose (minR, minC) (maxR, maxC) = S.fromList $ top <> right <> bottom <> left
  where
    cs = [minC - 1 .. maxC + 1]
    rs = [minR - 1 .. maxR + 1]
    top = [(minR - 1, c) | c <- cs]
    right = [(r, maxC + 1) | r <- rs]
    bottom = [(maxR + 1, c) | c <- cs]
    left = [(r, minC - 1) | r <- rs]

cycleGrid :: (Int, Int) -> (Int, Int) -> (Int, Int)
cycleGrid (maxR, maxC) (r, c) = (r', c')
  where
    outR = maxR + 1
    outC = maxC + 1
    r' = if r < 0 then outR - abs (r `rem` outR) else r `rem` outR
    c' = if c < 0 then outC - abs (c `rem` outC) else c `rem` outC

takeSteps :: ((Int, Int) -> Bool) -> Set (Int, Int) -> Set (Int, Int)
takeSteps canGo = S.fromList . concatMap (filter canGo . getNeighbors4) . S.toList

main' :: IO ()
main' = do
  contents <- parseGridAsIs <$> readFile "../inputs/2023/Day21/input.txt"
  let maxes = fst $ M.findMax contents
  let start = (fst . head . M.toList . M.filter (== 'S')) contents -- extract S point
  let grid = S.fromList $ M.keys $ M.filter (== '#') contents -- only need points that are '#'

  -- part 1
  let cantGo = S.union grid (enclose (0, 0) maxes) -- total cant go places
  let canGo = (`S.notMember` cantGo)
  let r = iterate (takeSteps canGo) (S.singleton start)
  print $ length $ r !! 64

  -- part 2
  let canGo2 p = cycleGrid maxes p `S.notMember` grid
  let r2 = iterate (takeSteps canGo2) (S.singleton start)
  print $ length $ r2 !! 26501365
  return ()
