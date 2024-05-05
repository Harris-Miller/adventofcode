module Day3 where

import Data.Bifunctor
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

data Symbol = Symbol Char SourcePos
  deriving (Show)

symbolSourcePos :: Symbol -> SourcePos
symbolSourcePos (Symbol _ pos) = pos

sourcePosToPoint :: SourcePos -> (Line, Column)
sourcePosToPoint pos = (sourceLine pos, sourceColumn pos)

data PartNum = PartNum Int SourcePos SourcePos
  deriving (Show)

partNumNum :: PartNum -> Int
partNumNum (PartNum num _ _) = num

partNumSourcePos :: PartNum -> (SourcePos, SourcePos)
partNumSourcePos (PartNum _ ePos sPos) = (ePos, sPos)

-- this gets not only the number, but it's starting line/col and ending line/col
collectNumbers :: Parser (Either PartNum Symbol)
collectNumbers = do
  sPos <- getPosition
  c <- read <$> many1 digit
  ePos <- getPosition -- this is actually the next pos
  let ePos' = setSourceColumn ePos (sourceColumn ePos - 1) -- so go back by one
  return $ Left (PartNum c sPos ePos')

-- get symbol with line/col
collectSymbol :: Parser (Either PartNum Symbol)
collectSymbol = do
  sPos <- getPosition
  c <- anyChar
  return $ Right (Symbol c sPos)

-- skipping over all 0-to-n '.' chars, collect numbers or symbols in-between
-- this _should_ skip newline '\n' chars along with '.', but I couldn't get that to work
collect :: Parser [Either PartNum Symbol]
collect = many $ do
  skipMany (char '.') -- parser fails with `skipMany (choice [char '.', endOfLine])`, unsure why
  choice [collectNumbers, collectSymbol]

calcNeighbors8 :: SourcePos -> [(Int, Int)]
calcNeighbors8 sPos = [(l', c') | l' <- [l - 1 .. l + 1], c' <- [c - 1 .. c + 1], (l', c') /= (l, c)]
  where
    l = sourceLine sPos
    c = sourceLine sPos

calcSourcePosPointRange :: (SourcePos, SourcePos) -> [(Int, Int)]
calcSourcePosPointRange (sPos, ePos) = [(sourceLine sPos, c) | c <- [sourceColumn sPos .. sourceColumn ePos]]

calcNeighborsAround2 :: SourcePos -> SourcePos -> [(Int, Int)]
calcNeighborsAround2 sPos ePos = (l, cs - 1) : (l, ce + 1) : [(l', c') | l' <- [l - 1, l + 1], c' <- [cs - 1 .. ce + 1]]
  where
    l = sourceLine sPos
    cs = sourceColumn sPos
    ce = sourceColumn ePos

filterPartNums :: ([PartNum], [Symbol]) -> [PartNum]
filterPartNums (ns, ss) = filter (hasSymbolNeighbors (map (sourcePosToPoint . symbolSourcePos) ss) . neighborsForPartNum) ns
  where
    neighborsForPartNum = uncurry calcNeighborsAround2 . partNumSourcePos
    hasSymbolNeighbors a b = not $ null $ intersect a b

filterPartNum2Star :: [(Int, Int)] -> PartNum -> Bool
filterPartNum2Star starPoints partNum = not $ null $ intersect starPoints theRange
  where
    theRange = (calcSourcePosPointRange . partNumSourcePos) partNum
    matches = not $ null $ intersect starPoints theRange

getPartNumsForStar :: [PartNum] -> Symbol -> [PartNum]
getPartNumsForStar ps star = filter ((starPos `elem`) . uncurry calcNeighborsAround2 . partNumSourcePos) ps
  where
    starPos = (sourcePosToPoint . symbolSourcePos) star
    surroundingPoints = map (second (uncurry calcNeighborsAround2 . partNumSourcePos) . (\p -> (p, p))) ps

part2 :: ([PartNum], [Symbol]) -> Int
part2 (ns, ss) = sum $ map (product . map partNumNum) gearPartNums
  where
    starSymbols = filter (\(Symbol n _) -> n == '*') ss
    gearPartNums = filter ((== 2) . length) $ map (getPartNumsForStar ns) starSymbols

main' :: IO ()
main' = do
  contents <- readFile "../inputs/2023/Day3/input.txt"
  -- part 1
  -- filtering on the newLine '\n' char is because of how I couldn't get skipping it in the `collect` function defined above
  let r = second (filter (\(Symbol n _) -> n /= '\n')) . partitionEithers <$> parse collect "" contents
  -- filter out nums that don't have an adjacent symbol, making them "PartNums" as defined in the puzzle
  let r2 = filterPartNums <$> r
  -- sum
  let r3 = sum . map (\(PartNum num _ _) -> num) <$> r2
  print r3

  -- part 2
  let starPoints = map (sourcePosToPoint . symbolSourcePos) . filter (\(Symbol n _) -> n /= '*') . snd <$> r

  let rr1 = part2 <$> r
  print rr1
