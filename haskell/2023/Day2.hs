module Day2 where

import Data.Bifunctor
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe

type Cube = (String, Int)

type Group = [Cube]

type Game = [Group]

-- parsing
parseCube :: String -> Cube
parseCube s = (color, read num)
  where
    [num, color] = splitOn " " s

parseGroup :: String -> Group
parseGroup = map parseCube . splitOn ", "

parseGame :: String -> (Int, Game)
parseGame s = (gameNum, groups)
  where
    [gameName, gameGroups] = splitOn ": " s
    gameNum = read $ drop 5 gameName
    groups = map parseGroup $ splitOn "; " gameGroups

-- part 1

isGroupPossible :: Group -> Group -> Bool
isGroupPossible toCheckAgainst = all checkCubes
  where
    checkCubes (color, num) = maybe False (num <=) (lookup color toCheckAgainst)

isGamePossible :: Group -> Game -> Bool
isGamePossible toCheckAgainst = all (isGroupPossible toCheckAgainst)

-- part 2

calcGamePower :: Game -> Int
calcGamePower = product . M.elems . foldr (uncurry $ M.insertWith max) M.empty . concat

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2023/Day2/input.txt"
  let games = map parseGame contents
  -- part 1
  let toCheckAgainst = [("red", 12), ("green", 13), ("blue", 14)]
  print $ sum $ map fst $ filter snd $ map (second (isGamePossible toCheckAgainst)) games
  -- part 2
  print $ sum $ map (calcGamePower . snd) games
