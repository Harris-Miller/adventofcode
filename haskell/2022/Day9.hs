module Day9 where

import Data.List

type Pt = (Int, Int)

type Rope = (Pt, Pt)

type Move = (Pt, Int)

parseDir :: String -> Pt
parseDir "U" = (0, 1)
parseDir "D" = (0, -1)
parseDir "L" = (-1, 0)
parseDir "R" = (1, 0)

parseInput :: String -> (Pt, Int)
parseInput x = (parseDir dir, read steps)
  where
    [dir, steps] = words x

startingRope :: Int -> [Pt]
startingRope n = replicate n (0, 0)

updateHead :: Pt -> Pt -> Pt
updateHead (x, y) (x', y') = (x + x', y + y')

updateTail :: Pt -> Pt -> Pt
updateTail (tx, ty) (hx, hy)
  | (dx, dy) == (2, 2) = (nx, ny)
  | dx == 2 = (nx, hy)
  | dy == 2 = (hx, ny)
  | otherwise = (tx, ty)
  where
    (dx, dy) = (abs (hx - tx), abs (hy - ty))
    nx = if hx > tx then hx - 1 else hx + 1
    ny = if hy > ty then hy - 1 else hy + 1

move :: Pt -> [Pt] -> [Pt]
move u (h : t) = scanl (flip updateTail) (updateHead u h) t

moveX :: Move -> [Pt] -> [[Pt]]
moveX (p, x) r = scanl (flip move) r $ replicate x p

start :: [Move] -> [[Pt]] -> [[Pt]]
start moves rope = foldl (\acc m -> acc <> tail (moveX m (last acc))) rope moves

main' :: IO ()
main' = do
  contents <- lines <$> readFile "../inputs/2022/Day9/input.txt"
  let moves = map parseInput contents

  -- part 1
  print $ length . nub . map last . start moves $ [startingRope 2]

  -- part 2
  print $ length . nub . map last . start moves $ [startingRope 10]
  return ()
