module Day7 where

import Control.Monad
import Control.Monad.RWS.Strict
import Data.Bifunctor
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as M
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.List
import Data.Maybe

type Edge = (Char, Char)

parse :: String -> Edge
parse s =
  let [_, a, _, _, _, _, _, b, _, _] = words s
   in (head a, head b)

toMap :: [Edge] -> HashMap Char [Char]
toMap xs =
  let allNodes = nub $ concatMap (\(a, b) -> [a, b]) xs
      start = M.fromList $ map (,[]) allNodes
   in M.map sort $ foldr (\(a, b) acc -> M.insertWith (<>) a [b] acc) start xs

collectStarts :: HashMap Char [Char] -> [Char]
collectStarts m =
  let keys = M.keys m
      values = (nub . sort . concat . M.elems) m
   in keys \\ values

topSort :: [Char] -> RWS (HashMap Char [Char]) [Char] (HashSet Edge) ()
topSort [] = return ()
topSort (current : todo) = do
  -- add to accum
  tell [current]

  -- get children
  childNodes <- reader (! current)

  -- remove edges from current to children
  let toRemove = S.fromList $ map (current,) childNodes
  nextGraph <- gets (`S.difference` toRemove)
  put nextGraph

  -- find those children who now no longer have any incoming edges
  let newTodo = filter (S.null . (\x -> S.filter ((== x) . snd) nextGraph)) childNodes
  -- add them to todo nad run again
  topSort $ sort (todo <> newTodo)

type RR = HashMap Char [Char]

type WW = (Sum Int)

data SS = SS {graph :: HashSet Edge, workers :: HashMap Int (Maybe (Char, Int))}
  deriving (Eq, Show)

nodeTime :: Char -> Int
-- nodeTime c = fromEnum c - 64 -- for sample
nodeTime c = fromEnum c - 4 -- for input

fakeAsyncTopSort :: HashSet Char -> RWST RR WW SS IO ()
fakeAsyncTopSort todos = do
  SS {..} <- get
  -- liftIO $ print todos
  let availWorkers = (M.keys . M.filter isNothing) workers
  let nodesToStart = S.filter (`notElem` S.map snd graph) todos

  let newWorkers = M.union (M.fromList $ zip availWorkers $ map (\n -> Just (n, nodeTime n)) (S.toList nodesToStart)) workers
  -- liftIO $ print (M.toList newWorkers)
  let remaining = todos `S.difference` (S.fromList . map fst . catMaybes . M.elems) newWorkers
  -- liftIO $ print remaining
  let timeToNextComplete = maybe 0 (minimum . map snd) $ ((\xs -> if null xs then Nothing else Just xs) <=< sequence . filter isJust . M.elems) newWorkers
  -- liftIO $ print timeToNextComplete
  tell (Sum timeToNextComplete)

  let newWorkers' = M.map (fmap (second (\x -> x - timeToNextComplete))) newWorkers
  let finishedWork = map fst . filter ((== 0) . snd) $ catMaybes $ M.elems newWorkers'
  -- liftIO $ print finishedWork
  let newWorkers'' = M.map (>>= \v -> if snd v == 0 then Nothing else Just v) newWorkers'
  -- liftIO $ print (M.toList newWorkers')

  nodeToNodes <- ask
  let nextGraph = foldr (\n acc -> S.filter ((/= n) . fst) acc) graph finishedWork

  let nextState = SS {graph = nextGraph, workers = newWorkers''}
  put nextState
  -- liftIO $ print remaining
  -- liftIO $ print nextState
  -- liftIO $ getLine >>= print
  if null remaining && all isNothing (M.elems newWorkers'') then return () else fakeAsyncTopSort remaining

main' :: IO ()
main' = do
  edges <- map parse . lines <$> readFile "../inputs/2018/Day7/input.txt"
  let edgesSet = S.fromList edges
  let nodeToNodes = toMap edges
  let starts = sort $ collectStarts nodeToNodes

  -- part 1
  let (_, _, w) = runRWS (topSort starts) nodeToNodes edgesSet
  print w

  -- part 2
  let allNodes = M.keysSet nodeToNodes
  (_, _, w) <- runRWST (fakeAsyncTopSort allNodes) nodeToNodes $ SS {graph = edgesSet, workers = M.fromList [(x, Nothing) | x <- [1 .. 5]]}
  print w

  return ()
