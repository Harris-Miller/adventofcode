module Day1 where

main' :: IO ()
main' = do
  xs <- map read . lines <$> readFile "../inputs/2020/Day1/input.txt" :: IO [Int]
  -- Part 1
  print $ uncurry (*) $ head [(x, y) | x <- xs, y <- xs, x + y == 2020]
  -- Part 2
  print $ (\(a, b, c) -> a * b * c) $ head [(x, y, z) | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
