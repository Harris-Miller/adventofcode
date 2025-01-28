module Day3 where

import Control.Monad.State.Strict as St
import Data.Either
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data Thing a = Do | Dont | Mul a
  deriving (Show)

skip :: GenParser Char st (Maybe (Thing (Int, Int)))
skip = anyChar >> return Nothing

parseMul :: GenParser Char st (Maybe (Thing (Int, Int)))
parseMul = do
  string "mul("
  a <- read <$> many1 digit
  char ','
  b <- read <$> many1 digit
  char ')'
  return $ Just (Mul (a, b))

parseDo :: GenParser Char st (Maybe (Thing (Int, Int)))
parseDo = do
  string "do()"
  return $ Just Do

parseDont :: GenParser Char st (Maybe (Thing (Int, Int)))
parseDont = do
  string "don't()"
  return $ Just Dont

doTheThing :: GenParser Char st [Thing (Int, Int)]
doTheThing = do
  first <- try parseMul <|> skip
  next <- try doTheThing <|> return []
  return $ maybe next (: next) first

doTheThing2 :: GenParser Char st [Thing (Int, Int)]
doTheThing2 = do
  first <- try parseDo <|> try parseDont <|> try parseMul <|> skip
  next <- try doTheThing2 <|> return []
  return $ maybe next (: next) first

calc2 :: [Thing (Int, Int)] -> St.State Bool [Int]
calc2 [] = return []
calc2 (x : xs) = do
  case x of
    Do -> put True >> calc2 xs
    Dont -> put False >> calc2 xs
    Mul (a, b) -> do
      c <- get
      if c then return ((a * b) : evalState (calc2 xs) c) else calc2 xs

main' :: IO ()
main' = do
  contents <- concat . lines <$> readFile "../inputs/2024/Day3/sample.txt"
  -- print contents
  let r = fromRight [] (parse doTheThing "Day3" contents)
  -- mapM_ print r
  print $ sum $ map (\(Mul (a, b)) -> a * b) r
  -- part 2
  let r2 = fromRight [] (parse doTheThing2 "Day3" contents)
  mapM_ print r2
  print $ sum $ evalState (calc2 r2) True
