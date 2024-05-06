module Day2 where

import Data.Functor

data Move = Rock | Paper | Scissor
  deriving (Show, Eq)

instance Ord Move where
  compare Rock Paper = LT
  compare Rock Rock = EQ
  compare Rock Scissor = GT
  compare Paper Scissor = LT
  compare Paper Paper = EQ
  compare Paper Rock = GT
  compare Scissor Rock = LT
  compare Scissor Scissor = EQ
  compare Scissor Paper = GT

charToMove :: Char -> Move
charToMove x
  | x == 'A' || x == 'X' = Rock
  | x == 'B' || x == 'Y' = Paper
  | x == 'C' || x == 'Z' = Scissor

readMove :: String -> (Move, Move)
readMove [f, _, s] = (charToMove f, charToMove s)

moveToValue :: Move -> Int
moveToValue Rock = 1
moveToValue Paper = 2
moveToValue Scissor = 3

orderingToInt :: Ordering -> Int
orderingToInt LT = 0
orderingToInt EQ = 3
orderingToInt GT = 6

winLoseDraw :: (Move, Move) -> Int
winLoseDraw (them, you) = orderingToInt $ compare you them

action :: (Move, Move) -> Int
action (them, you) = winLoseDraw (them, you) + moveToValue you

data Round = Win | Lose | Draw
  deriving (Show)

charToRound :: Char -> Round
charToRound 'X' = Lose
charToRound 'Y' = Draw
charToRound 'Z' = Win

readMoveRound :: String -> (Move, Round)
readMoveRound [f, _, s] = (charToMove f, charToRound s)

roundToMove :: (Move, Round) -> Move
roundToMove (Rock, Lose) = Scissor
roundToMove (Rock, Draw) = Rock
roundToMove (Rock, Win) = Paper
roundToMove (Paper, Lose) = Rock
roundToMove (Paper, Draw) = Paper
roundToMove (Paper, Win) = Scissor
roundToMove (Scissor, Lose) = Paper
roundToMove (Scissor, Draw) = Scissor
roundToMove (Scissor, Win) = Rock

action2 :: (Move, Round) -> Int
action2 x = action (fst x, roundToMove x)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2022/Day2/input.txt"
  -- part 1
  print $ sum $ map (action . readMove) contents
  -- part 2
  print $ sum $ map (action2 . readMoveRound) contents
