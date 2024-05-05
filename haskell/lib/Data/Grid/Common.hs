module Data.Grid.Common where

import Control.Monad.RWS

go :: (Char -> Bool) -> (Char -> a) -> String -> RWS () [((Int, Int), a)] (Int, Int) ()
go filterChar parseChar [] = return ()
go filterChar parseChar (x : xs) = do
  (l, c) <- get
  if x == '\n'
    then put (l + 1, 0)
    else when (filterChar x) (tell [((l, c), parseChar x)]) >> put (l, c + 1)
  go filterChar parseChar xs

collectGrid :: (Char -> Bool) -> (Char -> a) -> String -> [((Int, Int), a)]
collectGrid filterChar parseChar s = snd $ execRWS (go filterChar parseChar s) () (0, 0)

getNeighbors4 :: (Int, Int) -> [(Int, Int)]
getNeighbors4 (r, c) = [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

getNeighbors8 :: (Int, Int) -> [(Int, Int)]
getNeighbors8 (r, c) = [(r', c') | r' <- [r - 1, r, r + 1], c' <- [c - 1, c, c + 1], (r', c') /= (r, c)]
