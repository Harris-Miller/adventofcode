module Day20 where

import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Common

data ModuleType = Broadcaster | FlipFlop | Invert | Conjunction [String] | Untyped
  deriving (Show, Eq)

isConjunction :: ModuleType -> Bool
isConjunction (Conjunction _) = True
isConjunction _ = False

data ModuleBroadcaster = ModuleBroadcaster {broadcaster :: [String], modules :: Map String (ModuleType, [String])}
  deriving (Show)

emptyModuleBroadcaster :: ModuleBroadcaster
emptyModuleBroadcaster = ModuleBroadcaster {broadcaster = [], modules = M.empty}

parse :: ModuleBroadcaster -> String -> ModuleBroadcaster
parse mb = go . second (splitOn ", ") . toTuple . splitOn " -> "
  where
    go (name@(n : ns), v)
      | n == '%' = mb {modules = M.insert ns (FlipFlop, v) (modules mb)}
      | n == '&' = mb {modules = M.insert ns (Invert, v) (modules mb)}
      | name == "broadcaster" = mb {broadcaster = v}
      | otherwise = error "failed to parse"

updateConjunction :: ModuleBroadcaster -> ModuleBroadcaster
updateConjunction mb = mb {modules = nextModules}
  where
    mods = modules mb
    byFrom = foldl' (\acc (k, (_, ds)) -> foldl' (\acc2 d -> M.insertWith (<>) d [k] acc2) acc ds) M.empty (M.toList mods)
    nextModules = M.mapWithKey (\k (modType, ds) -> let fs = byFrom M.! k in if modType == Invert && length fs > 1 then (Conjunction fs, ds) else (modType, ds)) mods

runPulses :: ModuleBroadcaster -> (Map String Bool, [(String, Bool, String)])
runPulses mb = execRWS (tell (map ("broadcaster",False,) (broadcaster mb)) >> go (map (False,) (broadcaster mb))) () ffm
  where
    ffm = M.map (const False) $ M.filter ((== FlipFlop) . fst) (modules mb)
    ms = modules mb
    go :: [(Bool, String)] -> RWS () [(String, Bool, String)] (Map String Bool) ()
    go [] = return ()
    go ((p, x) : xs) = do
      ffState <- gets (M.lookup x)
      let (modType, destinations) = fromMaybe (Untyped, []) (M.lookup x ms)
      when (modType == Untyped) (modify (M.insertWith (||) x p))
      when (modType == FlipFlop && not p) (modify (M.adjust not x))
      let xss
            | modType == FlipFlop && not p = let nextP = not (fromJust ffState) in map (nextP,) destinations
            | modType == Invert = map (not p,) destinations
            | isConjunction modType = map (p,) destinations
            | otherwise = []

      tell $ map (\(p', x') -> (x, p', x')) xss
      go (xs <> xss)

main' :: IO ()
main' = do
  contents <- updateConjunction . foldl' parse emptyModuleBroadcaster . lines <$> readFile "../inputs/2023/Day20/sample2.txt"
  print contents
  let (s, w) = runPulses contents
  mapM_ print w
  print s
  return ()
