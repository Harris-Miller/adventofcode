module Day4_2 where

import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Tuple.Common (toTuple)

data ShiftRecord = ShiftRecord {time :: UTCTime, action :: String}
  deriving (Eq, Show)

instance Ord ShiftRecord where
  compare = comparing time

toMinutes :: NominalDiffTime -> Int
toMinutes = floor . (/ 60) . nominalDiffTimeToSeconds

toShiftRecord :: (UTCTime, String) -> ShiftRecord
toShiftRecord (t, a) = ShiftRecord {time = t, action = a}

parseDatetime :: String -> UTCTime
parseDatetime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"

splitTimeAndAction :: String -> ShiftRecord
splitTimeAndAction = toShiftRecord . bimap (parseDatetime . drop 1) (drop 2) . splitAt 17

-- each group of ShiftRecords starts awake, then alternates with "falls asleep" and "wakes up"
-- so just chunkBy 2 and do a time diff by minute and sum, to determine total sleep for that shift
calculateSleepCycles :: [ShiftRecord] -> (String, Int)
calculateSleepCycles (x : xs) = (guardNum, (toMinutes . sum) $ sleepBy xs)
  where
    guardNum = ((!! 1) . words . action) x
    sleepBy :: [ShiftRecord] -> [NominalDiffTime]
    sleepBy = map (uncurry (flip diffUTCTime) . toTuple) . chunksOf 2 . map time

collectBy :: [(String, Int)] -> HashMap String [Int]
collectBy = foldr (\(k, v) acc -> HM.alter (Just . (v :) . fromMaybe []) k acc) HM.empty

main' :: IO ()
main' = do
  content <- sortBy (comparing Down) . map splitTimeAndAction . lines <$> readFile "../inputs/2018/Day4/sample.txt"
  let grouped = map concat $ chunksOf 2 $ drop 1 $ reverse $ map reverse $ (split . whenElt) (isPrefixOf "Guard" . action) content
  let sleepByGuard = map calculateSleepCycles grouped
  let sleepGrouped = HM.toList $ HM.map sum $ collectBy sleepByGuard
  mapM_ print sleepGrouped
  return ()
