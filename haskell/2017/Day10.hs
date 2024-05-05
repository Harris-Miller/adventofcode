module Day10 where

import Data.Bifunctor
import Data.List.Split
import Data.Tuple
import Data.Tuple.Common
import Data.Tuple.Select

rotateR :: Int -> [a] -> [a]
rotateR n = uncurry (<>) . swap . uncurry splitAt . apTuple (mod n . length) id

rotateL :: Int -> [a] -> [a]
rotateL n = reverse . rotateR n . reverse

go :: (Int, Int, [a]) -> Int -> (Int, Int, [a])
go (s, i, acc) l = (s + 1, i + l + s, next')
  where
    acc' = rotateR i acc
    next = uncurry (<>) $ first reverse $ splitAt l acc'
    next' = rotateL i next

main' :: IO ()
main' = do
  lengths <- map read . splitOn "," . head . lines <$> readFile "2017/inputs/Day10/input.txt" :: IO [Int]
  let rope = [0 .. 255]
  let r = foldl go (0, 0, rope) lengths
  print $ (product . take 2 . sel3) r
  -- part 2
  return ()
