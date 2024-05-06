module Day8 where

import Control.Applicative qualified as M
import Control.Monad.State
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Common

readSigned :: String -> Int
readSigned s = let s' = if "+" `isPrefixOf` s then drop 1 s else s in read s'

processInst :: (String, Int) -> Int -> State (Int, [Int]) Int
processInst ("acc", val) acc = do
  (current, visited) <- get
  put (current + 1, current : visited)
  return (acc + val)
processInst ("jmp", val) acc = do
  (current, visited) <- get
  put (current + val, current : visited)
  return acc
processInst ("nop", val) acc = do
  (current, visited) <- get
  put (current + 1, current : visited)
  return acc

runInsts :: M.Map Int (String, Int) -> State (Int, [Int]) Int
runInsts m = go 0
  where
    go :: Int -> State (Int, [Int]) Int
    go acc = do
      (current, visited) <- get
      if current `elem` visited then return acc else processInst (fromJust $ M.lookup current m) acc >>= go

runInsts2 :: M.Map Int (String, Int) -> State (Int, [Int]) (Maybe Int)
runInsts2 m = go $ Just 0
  where
    go :: Maybe Int -> State (Int, [Int]) (Maybe Int)
    go Nothing = return Nothing
    go (Just acc) = do
      (current, visited) <- get
      if current `elem` visited then return Nothing else if M.notMember current m then return (Just acc) else processInst (fromJust $ M.lookup current m) acc >>= go . Just

flipOp :: (String, Int) -> (String, Int)
flipOp ("nop", a) = ("jmp", a)
flipOp ("jmp", a) = ("nop", a)

process2 :: M.Map Int (String, Int) -> [Int] -> (Int, (Int, [Int]))
process2 m = go
  where
    go :: [Int] -> (Int, (Int, [Int]))
    go (k : ks) = if isNothing a then go ks else (fromJust a, b)
      where
        m' = M.update (Just . flipOp) k m
        (a, b) = runState (runInsts2 m') (0, [])

main' :: IO ()
main' = do
  content <- M.fromList . zip [0 ..] . map (second readSigned . toTuple . splitOn " ") . lines <$> readFile "../inputs/2020/Day8/input.txt"
  -- Part 1
  let r = runState (runInsts content) (0, [])
  print r
  -- Part 2
  let keys = M.keys $ M.filter (\(op, _) -> op == "jmp" || op == "nop") content
  let r2 = process2 content keys
  print r2
