module Day4 where

import Crypto.Hash.MD5
import qualified Data.Text as T
import Data.Text.Encoding
import Text.Hex

toHex :: String -> T.Text
toHex = encodeHex . hash . encodeUtf8 . T.pack

process :: T.Text -> String -> Int -> Int
process prefix s n
  | prefix `T.isPrefixOf` toHex (s ++ show n) = n
  | otherwise = process prefix s (n + 1)

main' :: IO ()
main' = do
  contents <- head . lines <$> readFile "2015/inputs/Day4/input.txt"
  -- part 1
  print $ process "00000" contents 1
  -- part 2
  print $ process "000000" contents 1
