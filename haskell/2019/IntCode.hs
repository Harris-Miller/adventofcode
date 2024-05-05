module IntCode
  ( IntCode,
    processIntCode,
    processLoopMode,
  )
where

import Control.Monad.RWS
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tuple.GetSet
import Data.Tuple.Select

printT1 :: String -> IO ()
printT1 s = print $ "  " ++ s

type IntCode = RWST () [Int] ([Int], Int) IO (M.Map Int Int)

-- p :: (Int, [Int]) -> (Int, [Int])
-- p x = unsafePerformIO $ do
--   print x
--   return x

-- askNext :: RWS [Int] [Int] Int Int
-- askNext = do
--   (h, t) <- p . fromJust . uncons <$> ask
--   local (const t) $ return h

-- inc >>= return m

addOrMul :: (Int -> Int -> Int) -> (Int, Int, Int, Int) -> M.Map Int Int -> IntCode
addOrMul f (op, a', b', c') m = do
  i <- get2
  let a = fromJust $ M.lookup (i + 1) m >>= if a' == 1 then Just else flip M.lookup m
  let b = fromJust $ M.lookup (i + 2) m >>= if b' == 1 then Just else flip M.lookup m
  let c = fromJust $ M.lookup (i + 3) m
  put2 (i + 4)
  let r = a `f` b
  liftIO $ printT1 $ "Placing (" ++ show r ++ ") at (" ++ show c ++ ")"
  return $ M.insert c r m

consumeInput :: M.Map Int Int -> IntCode
consumeInput m = do
  (ins, i) <- get
  let (input, rest) = fromJust $ uncons $ ins
  let loc = fromJust $ M.lookup (i + 1) m
  liftIO $ printT1 $ "Consuming Input (" ++ show input ++ ")"
  put (rest, i + 2)
  return $ M.insert loc input m

produceOutput :: M.Map Int Int -> IntCode
produceOutput m = do
  i <- get2
  let loc = fromJust $ M.lookup (i + 1) m
  let out = fromJust $ M.lookup loc m
  liftIO $ printT1 $ "Producing Output (" ++ show out ++ ")"
  tell [out]
  put2 (i + 2)
  return m

jump :: (Int -> Int -> Bool) -> (Int, Int, Int, Int) -> M.Map Int Int -> IntCode
jump f (op, a', b', c') m = do
  i <- get2
  let a = fromJust $ M.lookup (i + 1) m >>= if a' == 1 then Just else flip M.lookup m
  let b = fromJust $ M.lookup (i + 2) m >>= if b' == 1 then Just else flip M.lookup m
  put2 (if a `f` 0 then i + 3 else b)
  return m

lessThanOrEqual :: (Int -> Int -> Bool) -> (Int, Int, Int, Int) -> M.Map Int Int -> IntCode
lessThanOrEqual f (op, a', b', c') m = do
  i <- get2
  let a = fromJust $ M.lookup (i + 1) m >>= if a' == 1 then Just else flip M.lookup m
  let b = fromJust $ M.lookup (i + 2) m >>= if b' == 1 then Just else flip M.lookup m
  let c = fromJust $ M.lookup (i + 3) m
  put2 (i + 4)
  let v = if a `f` b then 1 else 0
  return $ M.insert c v m

doTheThing :: (Int, Int, Int, Int) -> M.Map Int Int -> IntCode
doTheThing op m = do
  case sel1 op of
    1 -> addOrMul (+) op m
    2 -> addOrMul (*) op m
    3 -> consumeInput m
    4 -> produceOutput m
    5 -> jump (==) op m
    6 -> jump (/=) op m
    7 -> lessThanOrEqual (<) op m
    8 -> lessThanOrEqual (==) op m

padLeft :: String -> String
padLeft s = case length s of
  1 -> "0000" ++ s
  2 -> "000" ++ s
  3 -> "00" ++ s
  4 -> "0" ++ s
  _ -> s

parseOpcode :: Int -> (Int, Int, Int, Int)
parseOpcode x = (opcode, digitToInt a, digitToInt b, digitToInt c)
  where
    [x1, x2, a, b, c] = reverse $ padLeft $ show x
    opcode = read [x2, x1] :: Int

processIntCode :: M.Map Int Int -> IntCode
processIntCode m = do
  i <- get2
  let op = fromJust $ M.lookup i m
  if op == 99
    then return m
    else doTheThing (parseOpcode op) m >>= processIntCode

processLoopMode :: M.Map Int Int -> IntCode
processLoopMode m = do
  i <- get2
  liftIO $ printT1 $ "i: " ++ show i
  let opcode = parseOpcode $ fromJust $ M.lookup i m
  liftIO $ printT1 $ "Opcode: " ++ show opcode
  if sel1 opcode == 99
    then do
      liftIO $ print "Found termination, exiting..."
      return m
    else
      if sel1 opcode == 4
        then doTheThing opcode m
        else doTheThing opcode m >>= processLoopMode
