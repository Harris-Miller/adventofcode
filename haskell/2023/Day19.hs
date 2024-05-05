module Day19 where

import Control.Arrow
import Data.Either
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Range
import Data.Tuple.Common
import Text.Parsec
import Text.Parsec.String (Parser)

type Part = (String, Int)

data Rule = Rule String Char Int String | Goto String
  deriving (Show)

type Workflows = Map String [Rule]

parseOp :: Char -> Int -> Int -> Bool
parseOp '<' = (<)
parseOp '>' = (>)

parsecRule :: Parser Rule
parsecRule = do
  key <- letter
  op <- anyChar
  val <- read <$> many1 digit
  char ':'
  name <- many1 letter
  return $ Rule [key] op val name

parseRules :: String -> [Rule]
parseRules = rights . map go . splitOn ","
  where
    go s
      | ':' `notElem` s = Right $ Goto s
      | otherwise = parse parsecRule "" s

parseWorkflow :: String -> (String, [Rule])
parseWorkflow = second (parseRules . init) . toTuple . splitOn "{"

parseParts :: String -> [Part]
parseParts = map (second read . toTuple . splitOn "=") . splitOn "," . init . tail

parseInput :: String -> (Map String [Rule], [[Part]])
parseInput = (M.fromList . map parseWorkflow *** map parseParts . tail) . span (/= "") . lines

runWorkflow :: [Part] -> [Rule] -> String
runWorkflow parts = go
  where
    go [] = error "Fuck"
    go (Goto name : _) = name
    go (Rule key op val name : rs) = if parseOp op partVal val then name else go rs
      where
        partVal = fromJust $ lookup key parts

runEm :: Map String [Rule] -> [Part] -> Bool
runEm workflowsMap parts = go "in"
  where
    go "R" = False
    go "A" = True
    go name = go $ runWorkflow parts (workflowsMap M.! name)

thing :: Map String (Int, Int)
thing = M.fromList [("x", (1, 4000)), ("m", (1, 4000)), ("a", (1, 4000)), ("s", (1, 4000))]

data MinMax = MinMax Int Int
  deriving (Show)

instance Semigroup MinMax where
  (MinMax lMin lMax) <> (MinMax rMin rMax) = MinMax (max lMin rMin) (min rMax rMax)

ruleToMinMax :: Rule -> MinMax
ruleToMinMax (Rule _ op val _)
  | op == '<' = MinMax 1 val
  | otherwise = MinMax val 4000

determineAllPaths :: Map String [Rule] -> [Map String MinMax]
determineAllPaths wfs = map snd $ go (["in"], M.empty) $ wfs M.! "in"
  where
    go :: ([String], Map String MinMax) -> [Rule] -> [([String], Map String MinMax)]
    go acc@(ns, m) = concatMap go'
      where
        go' :: Rule -> [([String], Map String MinMax)]
        go' (Goto "R") = [] -- we don't care for Rs, only As
        go' (Goto "A") = [first (<> ["A"]) acc]
        go' (Goto name) = go acc $ wfs M.! name
        go' rule@(Rule key op val name)
          | name == "R" = [] -- we don't care for Rs, only As
          | name == "A" = [(ns <> ["A"], next)]
          | otherwise = go (ns <> [name], next) $ wfs M.! name
          where
            next = M.insertWith (<>) key (ruleToMinMax rule) m

main' :: IO ()
main' = do
  (workflows, parts) <- parseInput <$> readFile "../inputs/2023/Day19/sample.txt"
  -- part 1
  print $ sum $ map (sum . map snd) $ filter (runEm workflows) parts

  -- part 2
  let paths = determineAllPaths workflows
  mapM_ print paths
  putStrLn ""
  let ranges = foldl (\m (k, MinMax l r) -> M.insertWith (<>) k [l +=+ r] m) M.empty $ concatMap M.toList paths
  print ranges
  putStrLn ""
  let counts = M.map (map (\(SpanRange l r) -> boundValue r - boundValue l + 1) . mergeRanges) ranges
  print counts
  putStrLn ""
  let totalCounts = M.map sum counts
  print totalCounts
  putStrLn ""
  print $ product $ M.elems totalCounts
  -- let what3 = M.elems $ M.map (map (\(SpanRange l r) -> boundValue r - boundValue l) . mergeRanges) $ foldl (\m (k, MinMax l r) -> M.insertWith (<>) k [l +=+ r] m) M.empty $ concatMap M.toList paths
  -- print what3
  -- print $ product $ map sum what3
  -- let as = mapMaybe (M.lookup "a" . snd) what
  -- mapM_ print as
  return ()
