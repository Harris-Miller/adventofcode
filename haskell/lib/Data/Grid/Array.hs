{-# LANGUAGE FlexibleContexts #-}

module Data.Grid.Array
  ( module Data.Grid.Common,
    module Data.Grid.Array,
  )
where

import Data.Array (Array)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Grid.Common

type GridA e = Array (Int, Int) e

type GridUA e = UArray (Int, Int) e

-- can't use latest Data.Array since it breaks other packages
-- copy/pasted this from: https://hackage.haskell.org/package/array-0.5.7.0/docs/src/Data.Array.Base.html#genArray
genArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
genArray (l, u) f = listArray (l, u) $ map f $ range (l, u)

parseGrid :: (IArray a e) => (Char -> Bool) -> (Char -> e) -> String -> a (Int, Int) e
parseGrid filterChar parseChar s = array ((0, 0), (height, width)) $ collectGrid filterChar parseChar s
  where
    ss = lines s
    height = length ss - 1
    width = length (head ss) - 1

parseGridAsIs :: (IArray a Char) => String -> a (Int, Int) Char
parseGridAsIs = parseGrid (const True) id

-- Array and UArray already implement Show, so just have a -> String function instead
gridToListString :: (IArray a e) => (e -> Char) -> a (Int, Int) e -> [String]
gridToListString toChar grid = map (map (\a -> toChar (grid ! a)) . (\r -> [(c, r) | c <- [colStart .. colEnd]])) [rowStart .. rowEnd]
  where
    ((colStart, rowStart), (colEnd, rowEnd)) = bounds grid
