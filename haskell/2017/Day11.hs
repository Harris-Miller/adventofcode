module Day11 where

import Data.List.Split

data Cube a = Cube a a a
  deriving (Eq, Show)

instance (Num a) => Semigroup (Cube a) where
  (<>) (Cube q1 r1 s1) (Cube q2 r2 s2) = Cube (q1 + q2) (r1 + r2) (s1 + s2)

instance (Num a) => Monoid (Cube a) where
  mempty = Cube 0 0 0

cubeDistance :: (Integral a) => Cube a -> Cube a -> a
cubeDistance (Cube q1 r1 s1) (Cube q2 r2 s2) = (abs (q1 - q2) + abs (r1 - r2) + abs (s1 - s2)) `div` 2

readMove :: String -> Cube Int
readMove "nw" = Cube (-1) 0 1
readMove "n" = Cube 0 (-1) 1
readMove "ne" = Cube 1 (-1) 0
readMove "se" = Cube 1 0 (-1)
readMove "s" = Cube 0 1 (-1)
readMove "sw" = Cube (-1) 1 0

process :: Cube Int -> (Int, Cube Int) -> Cube Int -> (Int, Cube Int)
process start (longest, current) cube = (max longest distance, next)
  where
    next = current <> cube
    distance = cubeDistance start next

main' :: IO ()
main' = do
  contents <- splitOn "," . head . lines <$> readFile "../inputs/2017/Day11/input.txt"
  let start = Cube 0 0 0 :: Cube Int
  let final = mconcat (start : map readMove contents)
  print final
  print $ cubeDistance start final
  print $ fst $ foldl (process start) (0, start) (start : map readMove contents)
