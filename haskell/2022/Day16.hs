module Day16 where

import Control.Monad.Writer
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Select

type Valves = M.Map String Bool

type Neighbors = M.Map String [String]

type Flows = M.Map String Int

type PathWriter a = Writer [([String], Int)] a

type State = ([String], Valves, String, Int, Int)

parse :: String -> (String, Int, [String])
parse s = (id, flow, connections)
  where
    xs = words s
    id = xs !! 1
    flow = (read . (!! 1) . splitOn "=" . init . (!! 4)) xs
    connections = map (delete ',') $ drop 9 xs

getValve :: String -> Valves -> Bool
getValve p vs = fromJust $ M.lookup p vs

getNeighbors :: String -> Neighbors -> [String]
getNeighbors p ns = fromJust $ M.lookup p ns

getFlow :: String -> Flows -> Int
getFlow p fs = fromJust $ M.lookup p fs

getNextRate :: String -> Flows -> Int -> Int
getNextRate p fs r = r + getFlow p fs

createMoves :: Flows -> Neighbors -> State -> [State]
createMoves fs ns (ps, vs, p, r, c) = if c == 30 then [] else options
  where
    ns' = getNeighbors p ns
    nextRate = getNextRate p fs r
    self = [(ps, M.insert p False vs, p, nextRate, c + 1) | nextRate > 0]
    options = map (\n -> (ps ++ [n], vs, n, r, c + 1)) ns' <> self

process :: Flows -> Neighbors -> [([String], Int)] -> State -> [([String], Int)]
process fs ns acc state = if sel5 state == 30 then acc' else next
  where
    acc' = acc <> [(sel1 state, sel4 state)]
    moves = createMoves fs ns state
    next = foldl (process fs ns) acc moves

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2022/Day16/sample.txt"
  let valves = M.fromList $ map (\(name, _, _) -> (name, False)) contents :: Valves
  let neighbors = M.fromList $ map (\(name, _, cs) -> (name, cs)) contents :: Neighbors
  let flows = M.fromList $ map (\(name, flow, _) -> (name, flow)) contents :: Flows
  let state = (["AA"], valves, "AA", 0, 0)
  let r = process flows neighbors [] state
  -- print a
  print r
