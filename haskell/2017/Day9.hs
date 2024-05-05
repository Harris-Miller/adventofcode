module Day9 where

import Control.Monad
import Control.Monad.RWS
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid

removeCancelledChars :: String -> RWS () String Bool String
removeCancelledChars [] = return []
removeCancelledChars (x : xs) = do
  isCancelChar <- get
  if isCancelChar
    then put False >> removeCancelledChars xs
    else
      if x == '!'
        then put True >> removeCancelledChars xs
        else tell [x] >> removeCancelledChars xs

removeGarbage :: String -> RWS () String Bool String
removeGarbage [] = return []
removeGarbage (x : xs) = do
  isInGarbageSteam <- get
  if not isInGarbageSteam && x == '<'
    then put True >> removeGarbage xs
    else
      if x == '>'
        then put False >> removeGarbage xs
        else
          if isInGarbageSteam
            then removeGarbage xs
            else tell [x] >> removeGarbage xs

calculateGroups :: String -> RWS () [Int] Int String
calculateGroups [] = return []
calculateGroups (x : xs) = do
  if x == '{'
    then do
      level <- get
      let newLevel = level + 1
      put newLevel
      tell [newLevel]
      calculateGroups xs
    else
      if x == '}'
        then do
          level <- get
          let newLevel = level - 1
          put newLevel
          calculateGroups xs
        else calculateGroups xs

collectGarbage :: String -> RWS () (Sum Int) Bool String
collectGarbage [] = return []
collectGarbage (x : xs) = do
  isInGarbageSteam <- get
  if isInGarbageSteam
    then
      if x == '>'
        then put False >> collectGarbage xs
        else tell 1 >> collectGarbage xs
    else
      if x == '<'
        then put True >> collectGarbage xs
        else collectGarbage xs

main' :: IO ()
main' = do
  content <- head . lines <$> readFile "2017/inputs/Day9/input.txt"
  -- part 1
  let (_, _, s) = runRWS (removeCancelledChars content) () False
  let (_, _, s') = runRWS (removeGarbage s) () False
  let (_, _, levels) = runRWS (calculateGroups s') () 0
  -- print levels
  print $ sum levels
  -- part 2
  let (_, _, garbage) = runRWS (collectGarbage s) () False
  print $ getSum garbage
  return ()
