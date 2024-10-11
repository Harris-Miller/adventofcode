module Data.Grid.HashMap
  ( module Data.Grid.Common,
    module Data.Grid.HashMap,
  )
where

import Data.Grid.Common
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM

type Grid a = HashMap (Int, Int) a

parseGrid :: (Char -> Bool) -> (Char -> a) -> String -> Grid a
parseGrid filterChar parseChar = HM.fromList . collectGrid filterChar parseChar

parseGridAsIs :: String -> Grid Char
parseGridAsIs = parseGrid (const True) id
