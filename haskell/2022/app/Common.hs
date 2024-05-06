module Common where

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

twoByTwo :: [a] -> [(a, a)]
twoByTwo = go []
  where
    go acc [a, b] = acc <> [(a, b)]
    go acc (a : b : xs) = go (acc <> [(a, b)]) (b : xs)
