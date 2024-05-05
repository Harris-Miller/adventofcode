module Day7 where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

type Edge = (String, String)

parse :: String -> Edge
parse s = (a, b)
  where
    [_, a, _, _, _, _, _, b, _, _] = words s

toMap :: [Edge] -> Map String [String]
toMap xs = M.map sort $ foldr (\(a, b) acc -> M.insertWith (<>) a [b] acc) start xs
  where
    allNodes = nub $ concatMap (\(a, b) -> [a, b]) xs
    start = M.fromList $ map (,[]) allNodes

collectStarts :: Map String [String] -> [String]
collectStarts m = keys \\ values
  where
    keys = M.keys m
    values = (nub . sort . concat . M.elems) m

weirdTopSort :: [String] -> RWS (Map String [String]) [String] (Set Edge) ()
weirdTopSort [] = return ()
weirdTopSort (current : todo) = do
  -- add to accum
  tell [current]

  -- get children
  nodeToNodes <- ask
  let childNodes = nodeToNodes ! current

  -- remove edges from current to children
  graph <- get
  let toRemove = S.fromList $ map (current,) childNodes
  let nextGraph = S.difference graph toRemove
  put nextGraph

  -- find those children who now no longer have any incoming edges
  let newTodo = filter (S.null . (\x -> S.filter ((== x) . snd) nextGraph)) childNodes
  -- add them to todo nad run again
  weirdTopSort $ sort (todo <> newTodo)

getAllChildren :: Map String [String] -> String -> [String]
getAllChildren m s = m ! s <> concatMap (getAllChildren m) cs
  where
    cs = m ! s

main' :: IO ()
main' = do
  edges <- map parse . lines <$> readFile "../inputs/2018/Day7/input.txt"
  let edgesSet = S.fromList edges
  let nodeToNodes = toMap edges
  let starts = sort $ collectStarts nodeToNodes

  -- part 1
  let (_, _, w) = runRWS (weirdTopSort starts) nodeToNodes edgesSet
  print $ intercalate "" w

  return ()
