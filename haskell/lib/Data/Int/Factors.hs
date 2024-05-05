module Data.Int.Factors where

import Data.List

-- | brute force, very slow
calcFactors :: Int -> [Int]
calcFactors n = filter ((== 0) . mod n) [1 .. n]

-- | primes by trial division - https://wiki.haskell.org/Prime_numbers, search "Optimal trial division" header
primes :: [Int]
primes = 2 : [i | i <- [3 ..], and [rem i p > 0 | p <- takeWhile ((<= i) . (^ 2)) primes]]

-- | below stolen from: https://github.com/BartMassey/advent-of-code-2015/blob/master/20/Factor.hs

-- | Prime factors.
factors :: Int -> [Int]
factors 1 = [1]
factors n0 = f n0 primes
  where
    f _ [] = error "no factors"
    f n (c : cs)
      | n == c = [c]
      | n `mod` c == 0 = c : f (n `div` c) (c : cs)
      | otherwise = f n cs

-- | All factors including composites.
allFactors :: Int -> [Int]
allFactors n =
  processMultiples $ group $ factors n
  where
    multiples :: [Int] -> [Int]
    multiples g = map product $ inits g
    processMultiples :: [[Int]] -> [Int]
    processMultiples [] = []
    processMultiples [g] =
      multiples g
    processMultiples (g : gs) =
      let ms = multiples g
          fs = processMultiples gs
       in [m * f | f <- fs, m <- ms]
