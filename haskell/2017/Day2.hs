module Day2 where

parse :: String -> [Int]
parse = map read . words

main' :: IO ()
main' = do
  content <- map parse . lines <$> readFile "2017/inputs/Day2/input.txt"
  print $ sum $ map (\xs -> maximum xs - minimum xs) content
  print $ sum $ map (uncurry div . head . filter (\(x, y) -> x `mod` y == 0) . (\xs -> [(x, y) | x <- xs, y <- xs, x /= y])) content
