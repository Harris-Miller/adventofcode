module Day22 where

import Algorithm.Search
import Data.Grid.HashMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List
import Data.List.Common
import Data.Maybe
import Data.Point

data Node = Node {coords :: Point, size, used, avail :: Int}
  deriving (Eq, Show, Ord)

emptyNode :: Node -> Node
emptyNode node = node {used = 0, avail = size node}

addToNode :: Int -> Node -> Node
addToNode d node@(Node {..}) =
  let used' = used + d
      avail' = size - used'
   in node {used = used', avail = avail'}

parse :: String -> Node
parse s =
  let (xy, rest) = (fromJust . uncons . words . drop 16) s
      [(x, ys)] = (reads :: ReadS Int) xy
      y = read $ drop 2 ys
      [size', used', avail', _] = map (fst . head . (reads :: ReadS Int)) rest
   in Node {coords = Point x y, size = size', used = used', avail = avail'}

isViable :: (Node, Node) -> Bool
isViable (a, b) = used a /= 0 && a /= b && used a < avail b

type NodeGrid = HashMap Point Node

data SearchState = SearchState {emptyP :: Point, nodeGrid :: NodeGrid}
  deriving (Eq, Show, Ord)

findEmptyNode :: NodeGrid -> Point
findEmptyNode = coords . fromJust . find ((== Point 0 0) . coords) . HM.elems

genNeighbors :: SearchState -> [SearchState]
genNeighbors SearchState {..} =
  let emptyN = nodeGrid HM.! emptyP
      ps = map (nodeGrid HM.!) $ getNeighbors4P emptyP
      filtered = filter ((< size emptyN) . avail . (nodeGrid HM.!) . coords) ps
   in map (\n -> SearchState {emptyP = coords n, nodeGrid = HM.adjust emptyNode (coords n) $ HM.adjust (addToNode (used n)) emptyP nodeGrid}) filtered

found :: Point -> SearchState -> Bool
found p (SearchState {..}) =
  let n = nodeGrid HM.! p
   in used n == 0

findPathFromEmptyToPoint :: Point -> SearchState -> Maybe [SearchState]
findPathFromEmptyToPoint p = dfs genNeighbors (found p)

main' :: IO ()
main' = do
  contents <- map parse . drop 2 . lines <$> readFile "../inputs/2016/Day22/input.txt"
  let part1 = length . filter id . map isViable $ combosInclusive contents
  print part1

  let grid = HM.fromList $ map (\n -> (coords n, n)) contents :: NodeGrid
  let ss = SearchState {emptyP = findEmptyNode grid, nodeGrid = grid}
  let result = findPathFromEmptyToPoint (Point 1 0) ss

  -- print result

  -- part 2
  return ()
