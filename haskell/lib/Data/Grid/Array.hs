{-# LANGUAGE FlexibleContexts #-}

module Data.Grid.Array
  ( module Data.Grid.Common,
    module Data.Grid.Array,
  )
where

import Data.Array.IArray (IArray)
import Data.Array.Unboxed
import Data.Grid.Common

type GridA e = Array (Int, Int) e

type GridUA e = UArray (Int, Int) e

parseGrid :: (IArray a e) => (Char -> Bool) -> (Char -> e) -> String -> a (Int, Int) e
parseGrid filterChar parseChar s = array ((0, 0), (height, width)) $ collectGrid filterChar parseChar s
  where
    ss = lines s
    height = length ss - 1
    width = length (head ss) - 1

parseGridAsIs :: (IArray a Char) => String -> a (Int, Int) Char
parseGridAsIs = parseGrid (const True) id
