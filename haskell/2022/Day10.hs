module Day10 where

import Data.List
import Data.List.Split

-- the whole file is one of 3 things => 'noop', 'addx', and a number
-- since 'addx' takes 2 cycles, we can separate everything via `words`,
-- scan sum the whole thing together, treating 'noop' and 'addx' as 0s
processInput :: String -> [Int]
processInput = scanl (+) 1 . map parseWord . words
  where
    parseWord "addx" = 0
    parseWord "noop" = 0
    parseWord n = read n -- else is the number

renderYesNo :: (Int, Int) -> String
renderYesNo (cycle, x) = if (cycle `mod` 40) `elem` pixel then "#" else "."
  where
    pixel = [x, x + 1, x + 2]

main' :: IO ()
main' = do
  content <- readFile "../inputs/2022/Day10/input.txt"
  let states = processInput content
  let cycles = zip [1 ..] states
  -- part 1
  print $ sum $ map (uncurry (*)) $ filter ((== 20) . (`mod` 40) . fst) cycles
  -- part 2
  mapM_ print $ map (intercalate "") $ chunksOf 40 $ map renderYesNo $ init cycles
