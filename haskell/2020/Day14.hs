module Day14 where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Tuple.Common

type Addresses = M.Map Int Int

type Bits = M.Map Int Int

type Mask = [(Int, Int)]

to35 :: [Int] -> Bits
to35 xs = M.fromList $ zip [0 ..] (if l < 36 then replicate (36 - l) 0 <> xs else xs)
  where
    l = length xs

toNum :: Bits -> Int
toNum m = go (reverse $ M.elems m) 0
  where
    go [] _ = 0
    go (x : xs) n = (x * (2 ^ n)) + go xs (n + 1)

toBin :: (Num a2, Integral a1) => a1 -> [a2]
toBin 0 = [0]
toBin n = toBin (n `div` 2) ++ [if even n then 0 else 1]

applyMask :: Mask -> Bits -> Bits
applyMask mask v = foldr (\(k, v) acc -> M.insert k v acc) v mask

parseMask :: String -> Mask
parseMask = map (second digitToInt) . filter ((/= 'X') . snd) . zip [0 ..] . drop 7

parseMem :: String -> (Int, Bits)
parseMem = bimap (read . drop 4 . init) (to35 . toBin . read) . toTuple . splitOn " = "

parseSection :: [String] -> (Mask, [(Int, Bits)])
parseSection (m : xs) = (parseMask m, map parseMem xs)

processSection :: (Mask, [(Int, Bits)]) -> Addresses -> Addresses
processSection (mask, xs) addr = foldl (\acc (mem, v) -> M.insert mem (toNum $ applyMask mask v) acc) addr xs

main' :: IO ()
main' = do
  content <- map lines . split (startsWith "mask") <$> readFile "../inputs/2020/Day14/input.txt"
  let sections = map parseSection content
  -- mapM_ print $ map (second (map ((map intToDigit) . M.elems . snd))) sections
  let r = foldl (flip processSection) M.empty (map parseSection content)
  -- mapM_ print (M.toList r)
  print $ sum $ M.elems r
  return ()
