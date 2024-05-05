module Day15 where

import Control.Monad.IO.Class
import Data.List

process :: Int -> Int -> Int
process factor = (`rem` 2147483647) . (* factor)

processA :: Int -> Int
processA = process 16807

processB :: Int -> Int
processB = process 48271

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary n = toBinary (div n 2) ++ [mod n 2]

doIt :: Int -> [Int]
doIt = (reverse . take 16 . reverse) . toBinary

makeGeneratorSequence :: Int -> (Int -> Int -> Int) -> (Int -> Bool) -> [Int]
makeGeneratorSequence start fp shouldTake = filter shouldTake $ scanl fp start [0 ..]

countIfEqual :: Int -> (Int, Int) -> Int
countIfEqual acc (a, b) = acc + if doIt a == doIt b then 1 else 0

main' :: IO ()
main' = do
  let p1GenA = makeGeneratorSequence 699 (\last _ -> processA last) (\a -> (a `rem` 4) == 0)
  let p1GenB = makeGeneratorSequence 124 (\last _ -> processB last) (\a -> (a `rem` 8) == 0)
  let p1Seq = take 5_000_000 $ zip p1GenA p1GenB
  print $ foldl countIfEqual 0 p1Seq
