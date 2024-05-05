module Day6 where

import Control.Monad.RWS
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe

type Orbits = RWS () [String] Int String

countOrbits :: M.Map String String -> () -> String -> Orbits
countOrbits m _ x = do
  count <- get
  put (count + 1)
  let next = fromJust $ M.lookup x m
  tell [next]
  if next == "COM" then return next else countOrbits m () next

main' :: IO ()
main' = do
  contents <- map ((\[a, b] -> (b, a)) . splitOn ")") . lines <$> readFile "../inputs/2019/Day6/input.txt"
  let m = M.fromList contents
  -- part 1
  let orbits = map (\x -> (\(_, s, w) -> (x, (s, w))) $ runRWS (countOrbits m () x) () 0) $ M.keys m
  let results = sum $ map (fst . snd) orbits
  print results
  -- part2
  let you = zip [0 ..] $ snd $ fromJust $ lookup "YOU" orbits
  let san = zip [0 ..] $ snd $ fromJust $ lookup "SAN" orbits

  let crosses = snd $ head $ intersectBy (\y s -> snd y == snd s) you san
  let you' = fst $ fromJust $ find (\(_, a) -> a == crosses) you
  let san' = fst $ fromJust $ find (\(_, a) -> a == crosses) san
  print $ you' + san'
