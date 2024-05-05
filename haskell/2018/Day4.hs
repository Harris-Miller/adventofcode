module Day4 where

import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Time
import Data.Tuple.Common

parseDatetime :: String -> UTCTime
parseDatetime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"

getHour :: UTCTime -> String
getHour = formatTime defaultTimeLocale "%H"

-- specific to our Shift-Logs, Guards may start their shift in the 24rd hour of the previous day
-- in this case, normalize their start time to be midnight of the next date
-- add a day and reset to midnight, this effectively just moves the clock forward to beginning of next day
getShiftDay :: UTCTime -> Day
getShiftDay dt = utctDay (if getHour dt == "23" then addUTCTime nominalDay dt else dt)

data Shift = Shift {guardId :: Int, date :: String, sleepRanges :: [(String, String)]}

splitTimestampAndAction :: String -> (String, String)
splitTimestampAndAction = bimap (init . tail) tail . splitAt 18

partShiftStarts :: [String] -> ([String], [String])
partShiftStarts xs = (shiftStarts, rest)
  where
    (shiftStarts, rest) = partition ((== "Guard") . (!! 2) . words) xs

parseGuardId :: String -> Int
parseGuardId = read . tail . (!! 1) . words

data Action = Awake | Asleep
  deriving (Eq, Ord, Show)

parseAction :: String -> Action
parseAction "falls asleep" = Asleep
parseAction "wakes up" = Awake

parseSleepRanges :: [String] -> [(Day, [(DiffTime, DiffTime)])]
parseSleepRanges rest = keyed
  where
    ordered = map (parseDatetime . fst . splitTimestampAndAction) rest
    grouped = groupBy ((==) `on` utctDay) ordered
    keyed = map (\xs -> ((utctDay . head) xs, map toTuple $ chunksOf 2 $ map utctDayTime xs)) grouped

main' :: IO ()
main' = do
  content <- lines <$> readFile "2018/inputs/Day4/input.txt"
  let (shiftStarts, rest) = partShiftStarts content
  let dayToGuardL = map (bimap (getShiftDay . parseDatetime) parseGuardId . splitTimestampAndAction) shiftStarts
  -- map of day to Guard
  let dayToGuard = M.fromList dayToGuardL
  -- map of guard to days they are on shift
  let guardToDays = foldr (\(d, gId) -> M.insertWith (<>) gId [d]) M.empty dayToGuardL
  -- map of day to list of sleep ranges
  let daySleepRanges = M.fromList $ parseSleepRanges rest

  let totalSleptMinutes = M.toList $ M.map (sum . map (\(a, b) -> (`div` 60) $ numerator $ toRational (b - a)) . concatMap (daySleepRanges M.!)) guardToDays
  let guardAsleepTheMost = fst $ maximumBy (compare `on` snd) totalSleptMinutes
  print guardAsleepTheMost

  let daysForGuardThatSleptTheMost = guardToDays M.! guardAsleepTheMost
  let minuteMostSlept = head $ maximumBy (compare `on` length) $ group $ sort $ concatMap ((\(l, h) -> [l .. h - 1]) . tmap ((`div` 60) . numerator . toRational)) $ concatMap (daySleepRanges M.!) daysForGuardThatSleptTheMost
  print minuteMostSlept

  print $ guardAsleepTheMost * fromIntegral minuteMostSlept

  return ()
