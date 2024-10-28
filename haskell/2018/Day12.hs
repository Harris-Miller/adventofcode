module Day12 where

import Data.Bifunctor
import Data.List
import Data.Maybe

parse :: [String] -> ([Char], [(String, Char)])
parse xs =
  let s = (drop 15 . head) xs
      xs' = map (\a -> (take 5 a, last a)) $ drop 2 xs
   in (s, xs')

getSubSection :: [Char] -> Int -> [Char]
getSubSection s 0 = ".." ++ take 3 s
getSubSection s 1 = "." ++ take 4 s
getSubSection s i = (take 5 . drop (i - 2)) $ s ++ repeat '.'

applyRules :: [(String, Char)] -> [(Int, Char)] -> Int -> [(Int, Char)]
applyRules rules xs i
  | i >= length xs = []
  | otherwise =
      let e = xs !! i
          s = map snd xs
          sub = getSubSection s i
          found = maybe '.' snd (find ((== sub) . fst) rules)
       in second (const found) e : applyRules rules xs (i + 1)

-- trim :: String -> String
-- trim = dropWhile (== '.') . reverse . dropWhile (== '.') . reverse

nextGen :: [(String, Char)] -> [(Int, Char)] -> [(Int, Char)]
nextGen rules xs = applyRules rules xs 0

main' :: IO ()
main' = do
  (start, rules) <- first (zip [0 ..]) . parse . lines <$> readFile "../inputs/2018/Day12/input.txt"
  let xs = [(i, '.') | i <- [-20, -19 .. -1]] <> start <> [(i, '.') | i <- [length start .. length start + 20]]
  -- print xs
  let r = iterate' (nextGen rules) xs !! 20
  -- print $ last r
  -- mapM_ print $ map (map snd) r
  let rr = sum . map fst . filter ((== '#') . snd) $ r
  print rr

  let r2 = iterate' (nextGen rules) xs !! 50000000000
  let rr2 = sum . map fst . filter ((== '#') . snd) $ r2
  print rr2
  return ()
