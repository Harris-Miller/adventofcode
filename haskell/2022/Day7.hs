module Day7 where

import Control.Lens
import Data.Map qualified as M
import Data.Maybe
import ParseFS

calcDirSize :: Directory -> FS -> Int
calcDirSize dir fs = foldl (\acc sub -> acc + calcDirSize sub fs) localSize subDirs
  where
    localSize = sum $ map size $ view files dir
    subDirsS = view subDirectories dir
    subDirs = map (fromJust . (`M.lookup` fs)) (view subDirectories dir)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2022/day7/input.txt"
  let (_, fs) = parseKeyedFS contents
  let bySize = M.map (`calcDirSize` fs) fs
  -- part 1
  print $ sum $ filter (< 100000) $ M.elems bySize
  -- part 2
  let total = 70000000
  let needed = 30000000
  let remaining = total - calcDirSize (fromJust $ M.lookup "/" fs) fs
  print $ M.foldlWithKey (\acc k v -> if snd acc > v then (k, v) else acc) ("", total) $ M.filter ((> needed) . (+ remaining)) bySize
