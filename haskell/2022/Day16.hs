{-# LANGUAGE RecordWildCards #-}

module Day16 where

import Algorithm.Search
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.Split
import Data.Tuple.Select
import System.IO.Unsafe

-- (Value, Pressure, Connections)
parse :: String -> (String, (Int, [String]))
parse s =
  let (_ : valve : _ : _ : rate : _ : _ : _ : _ : conns) = words s
      rate' = fst $ head $ reads $ drop 5 rate :: Int
      conns' = splitOn "," $ concat conns
   in (valve, (rate', conns'))

type SearchRM a = ReaderT (Int, HashMap String (Int, [String])) IO a

data DState = DState
  { valve :: String,
    vState :: HashMap String Bool,
    releasing :: [Int]
  }
  deriving (Eq, Ord, Show)

next :: DState -> SearchRM [DState]
next dState@(DState {..}) = do
  (_, table) <- ask
  let (pressure, conns) = table HM.! valve
  let isOpen = vState HM.! valve
  let dState' = dState {releasing = head releasing : releasing}
  let openValve = [dState {vState = HM.insert valve True vState, releasing = pressure + head releasing : releasing} | not isOpen]
  return $ openValve <> [dState'] <> map (\c -> dState' {valve = c}) conns

cost :: DState -> DState -> SearchRM Int
cost from to = do
  (maxFlow, table) <- ask
  let didOpenValve = (valve from == valve to) && (not (vState from HM.! valve from) && vState to HM.! valve to)
  return $ maxFlow * length (releasing to) - sum (releasing to)

found :: DState -> SearchRM Bool
found dState@(DState {..}) = do
  -- lift $ print dState >> return dState
  lift $ print $ length releasing
  return $ length releasing == 30

main' :: IO ()
main' = do
  contents <- map parse . lines <$> readFile "../inputs/2022/Day16/sample.txt"
  mapM_ print contents
  let maxFlow = sum $ map (fst . snd) contents
  let valves = HM.fromList contents
  let readonly = (maxFlow, valves)
  let initialValveState = HM.fromList $ map (\t -> (fst t, False)) contents

  let m = dijkstraM next cost found $ DState {valve = "AA", vState = initialValveState, releasing = [0]}
  r <- runReaderT m readonly

  print r

  return ()
