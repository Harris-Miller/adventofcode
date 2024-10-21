module Day8 where

import Control.Monad.Writer

readNodeInfo :: [Int] -> ([Int], (Int, Int))
readNodeInfo (c : m : xs) = (xs, (c, m))

processChildren :: Int -> [Int] -> Writer (Sum Int) [Int]
processChildren 0 xs = return xs
processChildren x xs = do
  xs' <- processNode xs
  processChildren (x - 1) xs'

processNode :: [Int] -> Writer (Sum Int) [Int]
processNode xs = do
  let (x : m : rest) = xs
  rest' <- processChildren x rest
  let (ms, rest'') = splitAt m rest'
  tell $ mconcat (map Sum ms)
  return rest''

processTop :: [Int] -> Writer (Sum Int) ()
processTop [] = return ()
processTop xs = do
  xs' <- processNode xs
  processTop xs'

main' :: IO ()
main' = do
  contents <- map read . words . head . lines <$> readFile "../inputs/2018/Day8/input.txt" :: IO [Int]
  let r = execWriter $ processTop contents
  print r
