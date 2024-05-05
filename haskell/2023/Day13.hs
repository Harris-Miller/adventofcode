module Day13 where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple.Common

isReflection :: (Eq a) => [a] -> Int -> Bool
isReflection xs p = l' == reverse r'
  where
    (l, r) = splitAt p xs
    len = min (length l) (length r)
    (l', r') = drop (length l - len) *** take len $ (l, r)

findReflectionPoint :: [String] -> Maybe Int
findReflectionPoint xs = find (isReflection xs) [1 .. length xs - 1]

parsePatterns :: String -> ([String], [String])
parsePatterns = (id &&& transpose) . lines

main' :: IO ()
main' = do
  contents <- map parsePatterns . splitOn "\n\n" <$> readFile "../inputs/2023/Day13/input.txt"
  print $ sum $ map (fromJust . uncurry (<|>) . first (fmap (* 100)) . tmap findReflectionPoint) contents
