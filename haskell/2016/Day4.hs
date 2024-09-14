module Day4 where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Tuple.Common
import Data.Tuple.Extra

-- https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.List.html#unsnoc
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

flatten :: ((a, b), c) -> (a, b, c)
flatten ((a, b), c) = (a, b, c)

countBy :: (Hashable a) => [a] -> HM.HashMap a Int
countBy = foldr (\k acc -> HM.alter (Just . maybe 0 (+ 1)) k acc) HM.empty

doSort :: HM.HashMap a Int -> [(a, Int)]
doSort = sortBy (comparing (Down . snd)) . HM.toList

rotateLowercaseLetters :: Int -> Char -> Char
rotateLowercaseLetters 0 c = c
rotateLowercaseLetters n 'z' = rotateLowercaseLetters (n - 1) 'a'
rotateLowercaseLetters n c = rotateLowercaseLetters (n - 1) (succ c)

main' :: IO ()
main' = do
  contents <- map (flatten . first (fromJust . fmap (first (concat . intersperse "-")) . unsnoc . splitOn "-") . second init . toTuple . splitOn "[") . lines <$> readFile "../inputs/2016/Day4/input.txt"

  let filtered = filter ((\(a, b, c) -> c `isPrefixOf` a) . first3 (map fst . doSort . countBy . concat . splitOn "-")) contents
  mapM_ print filtered

  -- part 1
  let result1 = sum $ map (read . snd3) $ filtered
  print result1

  -- part 2
  let results2 = find (\(a, b, _) -> (unwords . map (map (rotateLowercaseLetters (read b))) $ splitOn "-" a) == "northpole object storage") filtered
  print $ snd3 $ fromJust results2

  return ()
