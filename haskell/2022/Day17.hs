module Day17 where

import Data.Grid.HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Point

main' :: IO ()
main' = do
  contents <- head . lines <$> readFile "../inputs/2022/Day17/sample.txt"
  print contents

  let grid = HM.fromList $ [(Point c r, "") | c <- [0 .. 6], r <- [0 .. 6]] :: HashMap Point String

  return ()
