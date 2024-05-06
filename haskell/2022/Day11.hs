module Day11 where

import Control.Lens
import Data.List
import Data.Tuple.Select

type Monkey = (Int -> Int, Int, Int, Int, [Int], Int)

input :: [Monkey]
input =
  [ ((* 3), 13, 6, 2, [89, 73, 66, 57, 64, 80], 0),
    ((+ 1), 3, 7, 4, [83, 78, 81, 55, 81, 59, 69], 0),
    ((* 13), 7, 1, 4, [76, 91, 58, 85], 0),
    ((^ 2), 2, 6, 0, [71, 72, 74, 76, 68], 0),
    ((+ 7), 19, 5, 7, [98, 85, 84], 0),
    ((+ 8), 5, 3, 0, [78], 0),
    ((+ 4), 11, 1, 2, [86, 70, 60, 88, 88, 78, 74, 83], 0),
    ((+ 5), 17, 3, 5, [81, 58], 0)
  ]

sample :: [Monkey]
sample =
  [ ((* 19), 23, 2, 3, [79, 98], 0),
    ((+ 6), 19, 2, 0, [54, 65, 75, 74], 0),
    ((^ 2), 13, 1, 3, [79, 60, 97], 0),
    ((+ 3), 17, 0, 1, [74], 0)
  ]

type Relief = Int -> Int

divBy :: Int -> Int -> Bool
divBy x = (== 0) . (`mod` x)

processItem :: Relief -> Int -> Int -> [Monkey] -> [Monkey]
processItem relief item i monkeys = nextGroup
  where
    monkey = monkeys !! i
    (op, test, ifTrue, ifFalse, items, count) = monkey
    worry = relief $ op item
    nI = if divBy test worry then ifTrue else ifFalse
    nMonkey = monkeys !! nI
    nItems = sel5 nMonkey <> [worry]
    nextGroup = set (ix nI . _5) nItems $ set (ix i . _5) (tail items) $ set (ix i . _6) (count + 1) monkeys

processMonkey :: Relief -> Int -> [Monkey] -> [Monkey]
processMonkey relief i monkeys = foldl (\acc item -> processItem relief item i acc) monkeys $ sel5 (monkeys !! i)

processMonkeys :: Relief -> [Monkey] -> [Monkey]
processMonkeys relief monkeys = foldl (flip (processMonkey relief)) monkeys [0 .. length monkeys - 1]

showItems :: [Monkey] -> IO ()
showItems x = do
  mapM_ (print . view _5) x
  putStrLn ""

main' :: IO ()
main' = do
  -- part 1
  let relief1 = flip div 3 :: Int -> Int
  print $ product . take 2 . reverse . sort . map (view _6) $ iterate (processMonkeys relief1) sample !! 20
  -- part 2
  let relief2 = id
  putStrLn "Round 1"
  mapM_ (print . view _6) $ iterate (processMonkeys relief2) sample !! 1
  putStrLn ""
  putStrLn "Round 20"
  mapM_ (print . view _6) $ iterate (processMonkeys relief2) sample !! 20
  putStrLn ""
  putStrLn "Round 1000"
  mapM_ (print . view _6) $ iterate (processMonkeys relief2) sample !! 1000
  putStrLn ""
  putStrLn "Round 10000"
  let inter = map (view _6) $ iterate (processMonkeys relief2) sample !! 10000
  mapM_ print inter
  putStrLn ""
  putStrLn "Total Monkey Business"
  print $ product . take 2 . reverse . sort $ inter
