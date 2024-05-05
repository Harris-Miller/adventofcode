module Day12 where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe
import Data.Scientific
import qualified Data.Vector as V

parse :: Bool -> Value -> Scientific
parse skipReds = addJson
  where
    addJson (Number n) = n
    addJson (Array a) = sum $ map addJson $ V.toList a
    addJson (Object o) | skipReds && String "red" `elem` KM.elems o = 0
    addJson (Object o) = sum $ map addJson $ KM.elems o
    addJson _ = 0

main' :: IO ()
main' = do
  contents <- decodeFileStrict "2015/inputs/Day12/input.txt" :: IO (Maybe Value)
  -- part 1
  print $ parse False $ fromJust contents
  -- part 2
  print $ parse True $ fromJust contents
