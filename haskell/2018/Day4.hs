module Day4 where

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
calculateSleepCycles :: [ShiftRecord] -> (String, [NominalDiffTime])
calculateSleepCycles (x : xs) = (guardNum, sleepBy xs)
  where
    guardNum = ((!! 1) . words . action) x
    sleepBy :: [ShiftRecord] -> [NominalDiffTime]
    sleepBy = map (uncurry (flip diffUTCTime) . toTuple) . chunksOf 2 . map time

-- each group of ShiftRecords starts awake, then alternates with "falls asleep" and "wakes up"
-- so just chunkBy 2 and do a time diff by minute and sum, to determine total sleep for that shift
sleepBy :: [ShiftRecord] -> (String, [NominalDiffTime])
sleepBy (x : xs) = (guardNum, sleepBy xs)
  where
    guardNum = ((!! 1) . words . action) x
    sleepBy :: [ShiftRecord] -> [NominalDiffTime]
    sleepBy = map (uncurry (flip diffUTCTime) . toTuple) . chunksOf 2 . map time

minuteByMinute :: (UTCTime, UTCTime) -> [DiffTime]
minuteByMinute (from, to) = map utctDayTime $ go from
  where
    go :: UTCTime -> [UTCTime]
    go t | t == to = []
    go t = t : go t'
      where
        t' = addUTCTime 60 t

breakdownSleepByMinute :: [ShiftRecord] -> (Int, [DiffTime])
breakdownSleepByMinute (x : xs) = (read $ drop 1 guardNum, ranges)
  where
    guardNum = ((!! 1) . words . action) x
    ranges = concatMap (minuteByMinute . toTuple) $ chunksOf 2 $ map time xs

collectBy :: [(Int, [DiffTime])] -> HashMap Int [DiffTime]
collectBy = foldr (\(k, v) acc -> HM.insertWith (<>) k v acc) HM.empty

main' :: IO ()
main' = do
  content <- sortBy (comparing Down) . map splitTimeAndAction . lines <$> readFile "../inputs/2018/Day4/input.txt"
  let grouped = map concat $ chunksOf 2 $ drop 1 $ reverse $ map reverse $ (split . whenElt) (isPrefixOf "Guard" . action) content

  -- test
  let sleepByGuard = map calculateSleepCycles grouped
  let sleepGrouped = HM.toList $ HM.map sum $ foldr (\(k, v) acc -> HM.insertWith (<>) k v acc) HM.empty sleepByGuard
  let guardWhoSleepsTheMost = read $ drop 1 $ fst $ maximumBy (comparing snd) sleepGrouped :: Int
  print guardWhoSleepsTheMost

  -- test2
  let sleepByMinute = collectBy $ map breakdownSleepByMinute grouped
  let sleepByMinute2 = HM.filter (not . null) $ HM.map (map (\ts -> (head ts, length ts)) . group . sort) sleepByMinute
  let sleepByMinute3 = HM.toList $ HM.map (fst . maximumBy (comparing snd)) sleepByMinute2

  let minuteSleptMost = fromInteger $ (`div` 60) $ (`div` 1_000_000_000_000) $ diffTimeToPicoseconds $ fromJust $ lookup guardWhoSleepsTheMost sleepByMinute3
  print minuteSleptMost

  print $ guardWhoSleepsTheMost * minuteSleptMost

  return ()
