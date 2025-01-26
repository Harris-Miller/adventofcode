module Day3 where

import Data.Either
import Data.List
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

-- mul :: GenParser Char st [String]
-- mul = do
--   result <- string "mul("
--   return result

parseNums :: GenParser Char st [(Int, Int)]
parseNums = do
  a <- read <$> many1 digit
  char ','
  b <- read <$> many1 digit
  char ')'
  return [(a, b)]

doTheThing :: GenParser Char st [(Int, Int)]
doTheThing = do
  manyTill anyChar (try (string' "mul("))
  first <- try parseNums <|> return []
  next <- try doTheThing <|> return []
  return (first <> next)

main' :: IO ()
main' = do
  contents <- concat . lines <$> readFile "../inputs/2024/Day3/input.txt"
  -- print contents
  let r = fromRight [] (parse doTheThing "Day3" contents)
  -- mapM_ print r
  print $ sum $ map (uncurry (*)) r
