module Day7 where

import Control.Monad.RWS
import Data.List
import Data.List.Split
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Tuple.Select
import Data.Tuple.Update
import IntCode

what :: IntCode -> IO Int -> Int -> IO Int
what theM acc x = do
  acc' <- acc
  last . sel3 <$> runRWST theM () ([x, acc'], 0)

doTheThing :: IntCode -> [Int] -> IO [Int]
doTheThing theM ps = sequenceA $ tail $ scanl (what theM) (return 0) ps

next :: Int -> Int
next 4 = 0
next i = i + 1

toSel :: Int -> ((a, a, a, a, a) -> a)
toSel 0 = sel1
toSel 1 = sel2
toSel 2 = sel3
toSel 3 = sel4
toSel 4 = sel5

toUpd :: Int -> (a -> (a, a, a, a, a) -> (a, a, a, a, a))
toUpd 0 = upd1
toUpd 1 = upd2
toUpd 2 = upd3
toUpd 3 = upd4
toUpd 4 = upd5

-- doTheThing2 ::
--   (Int, Int, Int, Int, Int) ->
--   Int ->
--   (Int, Int, Int, Int, Int) ->
--   [Int] ->
--   (M.Map Int Int, M.Map Int Int, M.Map Int Int, M.Map Int Int, M.Map Int Int) ->
--   IO Int
-- doTheThing2 ps p is ins ms = if op == 99 then last outs else doTheThing2 ps p' is' outs ms'
--   where
--     m = toSel p ms
--     theM = return m :: IntCode
--     (m', (_, i'), outs) = runRWS theM () (ins, toSel p ps)
--     op = fromJust $ M.lookup i' m'
--     ms' = toUpd p m' ms
--     is' = toUpd p i' is
--     p' = next p

type Amp = (Int, [Int], Int)

doTheThing2 ::
  Int ->
  ([Int], [Int], [Int], [Int], [Int]) ->
  (Int, Int, Int, Int, Int) ->
  (M.Map Int Int, M.Map Int Int, M.Map Int Int, M.Map Int Int, M.Map Int Int) ->
  IO [Int]
doTheThing2 loc ins is ms = do
  liftIO $ print $ "Amp: " ++ show loc
  let i = toSel loc is
  let m = toSel loc ms
  let theM = processLoopMode $ toSel loc ms :: IntCode
  (m', (_, i'), outs) <- runRWST theM () (toSel loc ins, i)
  let loc' = next loc
  let ins' = toUpd loc' (toSel loc' ins <> outs) $ toUpd loc [] ins
  let is' = toUpd loc i' is
  let ms' = toUpd loc m' ms
  let op = fromJust $ M.lookup i' m'
  liftIO $ print $ "Op: " ++ show op
  if op == 99 then return outs else doTheThing2 loc' ins' is' ms'

doTheThing3 ::
  Int ->
  (Int, Int, Int, Int, Int) ->
  (Int, Int, Int, Int, Int) ->
  (M.Map Int Int, M.Map Int Int, M.Map Int Int, M.Map Int Int, M.Map Int Int) ->
  IO [Int]
doTheThing3 loc (in1, in2, in3, in4, in5) = doTheThing2 loc ins'
  where
    ins' = ([in1, 0], [in2], [in3], [in4], [in5])

main' :: IO ()
main' = do
  contents <- zip [0 ..] . map read . splitOn "," . head . lines <$> readFile "../inputs/2019/Day7/input.txt" :: IO [(Int, Int)]
  let m = M.fromList contents
  let theM = processIntCode m
  -- part 1
  let perms = permutations [0, 1, 2, 3, 4]
  -- print perms
  -- rs <- last <$> doTheThing theM [4, 3, 2, 1, 0]
  rs <- maximum <$> traverse (fmap last . doTheThing theM) perms
  print rs

  -- part 2
  let contents = zip [0 ..] [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]
  let m = M.fromList contents
  rs <- doTheThing3 0 (9, 8, 7, 6, 5) (0, 0, 0, 0, 0) (m, m, m, m, m)
  print rs
