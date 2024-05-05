module Day23 where

import Control.Lens
import Data.List
import Data.Tuple.Select
import System.IO.Unsafe

_ab "a" = _1
_ab "B" = _2

dropPlus x = if head x == '+' then tail x else x

doIt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int, Int, Int) -> (Int, Int, Int)
doIt fa fb fp (a, b, p) = (fa a, fb b, fp p)

hlf :: String -> (Int, Int, Int) -> (Int, Int, Int)
hlf "a" state = doIt (`div` 2) id (+ 1) state
hlf "b" state = doIt id (`div` 2) (+ 1) state

tpl :: String -> (Int, Int, Int) -> (Int, Int, Int)
tpl "a" state = doIt (* 3) id (+ 1) state
tpl "b" state = doIt id (* 3) (+ 1) state

inc :: String -> (Int, Int, Int) -> (Int, Int, Int)
inc "a" state = doIt (+ 1) id (+ 1) state
inc "b" state = doIt id (+ 1) (+ 1) state

jmp :: Int -> (Int, Int, Int) -> (Int, Int, Int)
jmp x = doIt id id (+ x)

jie :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
jie "a" x state = jmp (if even $ sel1 state then x else 1) state
jie "b" x state = jmp (if even $ sel2 state then x else 1) state

jio :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
jio "a" x state = jmp (if 1 == sel1 state then x else 1) state
jio "b" x state = jmp (if 1 == sel2 state then x else 1) state

doOp :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
doOp instructions state@(a, b, i)
  | "hlf" `isPrefixOf` instruction = hlf (last $ words instruction) state
  | "tpl" `isPrefixOf` instruction = tpl (last $ words instruction) state
  | "inc" `isPrefixOf` instruction = inc (last $ words instruction) state
  | "jmp" `isPrefixOf` instruction = jmp (read . dropPlus . last $ words instruction) state
  | "jie" `isPrefixOf` instruction = (\[_, x, i] -> jie (inits x !! 1) ((read . dropPlus) i) state) (words instruction)
  | "jio" `isPrefixOf` instruction = (\[_, x, i] -> jio (inits x !! 1) ((read . dropPlus) i) state) (words instruction)
  where
    instruction = instructions !! i

run :: [String] -> IO (Int, Int, Int) -> IO (Int, Int, Int)
run instructions stateIO = do
  state <- stateIO
  -- print $ instructions !! sel3 state
  let next = doOp instructions state
  -- print next
  if length instructions <= sel3 next then return next else run instructions (return next)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2015/inputs/Day23/input.txt"
  result1 <- run contents (pure (0, 0, 0))
  print $ sel2 result1
  result2 <- run contents (pure (1, 0, 0))
  print $ sel2 result2
