module Day13 where

import Control.Monad.Writer
import Data.Bifunctor
import Data.Char
import Data.List.Split (splitOn)
import Data.Tuple.Common

data Packet a = Packet a | PacketList [Packet a]
  deriving (Show, Eq)

parse :: String -> Writer [Packet Int] String
parse [] = return ""
parse ('[' : xs) = do
  let (a, w) = runWriter (parse xs)
  tell [PacketList w]
  parse a
parse (',' : xs) = parse xs
parse (']' : xs) = return xs
parse (x : xs) = tell [Packet (digitToInt x)] >> parse xs

comparePackets :: Packet Int -> Bool
comparePackets = undefined

main' :: IO ()
main' = do
  contents <- map (bimap (init . tail) (init . tail) . toTuple . lines) . splitOn "\n\n" <$> readFile "../inputs/2022/Day13/sample.txt"
  let parsed = map (bimap (execWriter . parse) (execWriter . parse)) contents
  -- mapM_ print parsed
  return ()
