module Day5 where

import Control.Lens
import Data.List
import Data.Maybe
import Data.Stack

-- unsafe Stack operations
stackPop' :: Stack a -> (Stack a, a)
stackPop' = fromJust . stackPop

stackPeek' :: Stack a -> a
stackPeek' = fromJust . stackPeek

-- parse input line
parseMove :: String -> (Int, Int, Int)
parseMove s = (read x, read from, read to)
  where
    [_, x, _, from, _, to] = words s

listToStack :: [a] -> Stack a
listToStack = foldl stackPush stackNew

-- sample starting stack
--     [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3

-- this is part of the input, it is the visualized stacks initial value

--     [C]             [L]         [T]
--     [V] [R] [M]     [T]         [B]
--     [F] [G] [H] [Q] [Q]         [H]
--     [W] [L] [P] [V] [M] [V]     [F]
--     [P] [C] [W] [S] [Z] [B] [S] [P]
-- [G] [R] [M] [B] [F] [J] [S] [Z] [D]
-- [J] [L] [P] [F] [C] [H] [F] [J] [C]
-- [Z] [Q] [F] [L] [G] [W] [H] [F] [M]
--  1   2   3   4   5   6   7   8   9

-- sample Stacks
stacksSamples :: [Stack Char]
stacksSamples =
  [ listToStack ['Z', 'N'],
    listToStack ['M', 'C', 'D'],
    listToStack ['P']
  ]

-- this is the initial Stacks as visualized above
stacksActual :: [Stack Char]
stacksActual =
  [ listToStack ['Z', 'J', 'G'],
    listToStack ['Q', 'L', 'R', 'P', 'W', 'F', 'V', 'C'],
    listToStack ['F', 'P', 'M', 'C', 'L', 'G', 'R'],
    listToStack ['L', 'F', 'B', 'W', 'P', 'H', 'M'],
    listToStack ['G', 'C', 'F', 'S', 'V', 'Q'],
    listToStack ['W', 'H', 'J', 'Z', 'M', 'Q', 'T', 'L'],
    listToStack ['H', 'F', 'S', 'B', 'V'],
    listToStack ['F', 'J', 'Z', 'S'],
    listToStack ['M', 'C', 'D', 'P', 'F', 'H', 'B', 'T']
  ]

stacks = stacksActual

viewStack :: Int -> [Stack Char] -> Stack Char
viewStack i = view (ix (i - 1))

setStack :: Int -> Stack Char -> [Stack Char] -> [Stack Char]
setStack i = set (ix (i - 1))

transfer :: (Int, Int, Int) -> [Stack Char] -> (Stack Char, Stack Char)
transfer (n, from, to) xs = go n fromStack toStack
  where
    fromStack = viewStack from xs
    toStack = viewStack to xs
    go 0 fromStack toStack = (fromStack, toStack)
    go n fromStack toStack = go (n - 1) fromStack' toStack'
      where
        (fromStack', c) = stackPop' fromStack
        toStack' = stackPush toStack c

moveCrates :: (Int, Int, Int) -> [Stack Char] -> [Stack Char]
moveCrates d@(_, from, to) xs = setStack from fromStack $ setStack to toStack xs
  where
    (fromStack, toStack) = transfer d xs

transfer2 :: (Int, Int, Int) -> [Stack Char] -> (Stack Char, [Char])
transfer2 (n, from, to) xs = go n fromStack []
  where
    fromStack = viewStack from xs
    toStack = viewStack to xs
    go 0 fromStack temp = (fromStack, temp)
    go n fromStack temp = go (n - 1) fromStack' temp'
      where
        (fromStack', c) = stackPop' fromStack
        temp' = c : temp

moveCrates2 :: (Int, Int, Int) -> [Stack Char] -> [Stack Char]
moveCrates2 d@(_, from, to) xs = setStack from fromStack $ setStack to toStack xs
  where
    (fromStack, temp) = transfer2 d xs
    toStack = foldl' stackPush (viewStack to xs) temp

main' :: IO ()
main' = do
  contents <- map parseMove . lines <$> readFile "../inputs/2022/Day5/input.txt"
  let r1 = foldl' (flip moveCrates) stacks contents
  print $ map stackPeek' r1

  let r2 = foldl' (flip moveCrates2) stacks contents
  print $ map stackPeek' r2
