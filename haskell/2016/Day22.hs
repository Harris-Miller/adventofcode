module Day22 where

import Data.List
import Data.List.Common (pairsOrdered)
import Data.Maybe
import Data.Point

data Node = Node {coords :: Point, size, used, avail, useP :: Int}
  deriving (Eq, Show)

parse :: String -> Node
parse s =
  let (xy, rest) = (fromJust . uncons . words . drop 16) s
      [(x, ys)] = (reads :: ReadS Int) xy
      y = read $ drop 2 ys
      [size', used', avail', useP'] = map (fst . head . (reads :: ReadS Int)) rest
   in Node {coords = Point x y, size = size', used = used', avail = avail', useP = useP'}

isViable :: (Node, Node) -> Bool
isViable (a, b) = used a /= 0 && a /= b && used a < avail b

main' :: IO ()
main' = do
  contents <- map parse . drop 2 . lines <$> readFile "../inputs/2016/Day22/input.txt"
  let part1 = length . filter id . map isViable $ pairsOrdered contents
  print part1
  return ()
