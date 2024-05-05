module ParsecExample where

import Data.Either (fromRight)
import Text.Parsec
import Text.Parsec.String (Parser)

thing :: Parser (String, Int)
thing = do
  skipMany (space <|> char ',')
  c <- many1 digit
  spaces
  color <- choice [string "blue", string "red", string "green"]
  return (color, read c)

gameParser :: Parser (Int, [[(String, Int)]])
gameParser = do
  string "Game "
  c <- many1 digit
  string ":"
  group <- sepBy (sepBy thing (string ", ")) (string "; ")
  return (read c, group)

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2023/Day2/sample.txt"
  let r = parse gameParser "" (head contents)
  print r
  return ()
