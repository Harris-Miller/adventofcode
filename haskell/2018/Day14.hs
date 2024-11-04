module Day14 where

import Control.Monad.State
import Data.Char
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.Maybe

puzzleInput = 380621 :: Int

nextInLoop :: Int -> Int -> Int -> Int
nextInLoop cur toAdd l = (cur + toAdd) `mod` l

addToList :: IntMap Int -> Int -> IntMap Int
addToList xs v =
  let a = IntMap.fromAscList $ zip [length xs ..] $ map digitToInt (show v)
   in xs `IntMap.union` a

doTheThing :: State (IntMap Int, (Int, Int)) ()
doTheThing = do
  (xs, (i1, i2)) <- get
  let v1 = xs IntMap.! i1
  let v2 = xs IntMap.! i2
  let xs' = addToList xs (v1 + v2)
  let len = length xs'
  let i1' = nextInLoop i1 (1 + v1) len
  let i2' = nextInLoop i2 (1 + v2) len
  put (xs', (i1', i2'))
  return ()

part1 :: Int -> State (IntMap Int, (Int, Int)) ()
part1 lengthNeeded = do
  doTheThing
  (xs, (i1, i2)) <- get
  let len = length xs
  if len > lengthNeeded then return () else part1 lengthNeeded

findSubSection :: [Int] -> [(Int, Int)] -> Maybe Int
findSubSection a xs
  | length xs < length a = Nothing
  | otherwise =
      let xs' = take (length a) $ map snd xs
       in if a == xs' then (Just . fst . head) xs else findSubSection a (tail xs)

main' :: IO ()
main' = do
  let start = IntMap.fromAscList $ zip [0 ..] [3, 7]
  let indexes = (0, 1)
  let amount = puzzleInput

  -- part 1
  -- let r1 = execState (part1 (amount + 10)) (start, indexes)
  -- print $ map intToDigit $ take 10 $ drop amount $ IntMap.elems $ fst r1

  -- part 2
  -- let r2 = evalState (part2 (map digitToInt "380621")) (start, indexes)
  let r2 = execState (part1 (amount * 2)) (start, indexes)
  let r2' = findSubSection (map digitToInt "380621") $ IntMap.toAscList (fst r2)
  print r2'
  return ()
