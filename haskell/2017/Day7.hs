module Day7 where

import Data.List
import Data.List.Split
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple.Select

data Program = Program {name :: String, weight :: Int, holding :: [String]}
  deriving (Eq, Show)

parseNameAndWeight :: String -> Program
parseNameAndWeight s = Program {name = n, weight = w', holding = []}
  where
    [n, w] = splitOn " " s
    w' = (read . init . tail) w

parseProgram :: String -> Program
parseProgram s = go s'
  where
    s' = splitOn " -> " s
    go :: [String] -> Program
    go [nw] = parseNameAndWeight nw
    go [nw, hs] = p {holding = splitOn ", " hs}
      where
        p = parseNameAndWeight nw

-- just need to find the one that isn't being held up by any other
findBottomProgram :: [Program] -> String
findBottomProgram p = head $ allNames \\ heldNames
  where
    allNames = map name p
    heldNames = nub $ concatMap holding p

determineTotalWeight :: Map String Program -> Program -> Int
determineTotalWeight m p = go (holding p) (weight p)
  where
    go [] acc = acc
    go (x : xs) acc = go xs $ weight p' + go (holding p') acc
      where
        p' = m ! x

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

main' :: IO ()
main' = do
  contents <- map parseProgram . lines <$> readFile "2017/inputs/Day7/input.txt"
  -- part 1
  print $ findBottomProgram contents
  -- part 2
  let m = M.fromList $ map (\p -> (name p, p)) contents
  let weights = M.map (determineTotalWeight m) m
  let holdings = M.map (map (weights !) . holding) m
  print $ M.toList $ M.filter (not . allTheSame) $ M.filter (not . null) holdings
  let (n, hs) = head $ M.toList $ M.filter (not . allTheSame) $ M.filter (not . null) holdings
  let [is, shouldBe] = (concatMap nub . group . sort) hs
  return ()
