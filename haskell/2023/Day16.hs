module Day16 where

import Control.Monad.RWS
import Data.Array.Base
import Data.Grid.Array
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Ix (Ix (inRange, unsafeIndex))

data Direction = North | East | South | West
  deriving (Eq, Show, Ord, Enum)

data Beam = Beam Direction (Int, Int)
  deriving (Eq, Show, Ord)

-- Getting a dependency issue between array0.5.4.0 and array0.5.6.0 where this function was introduced
-- so just copy/pasting it from source: https://hackage.haskell.org/package/array-0.5.6.0/docs/src/Data.Array.Base.html#%21%3F

-- | Returns 'Just' the element of an immutable array at the specified index,
-- or 'Nothing' if the index is out of bounds.
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i =
  let b = bounds arr
   in if inRange b i
        then Just $ unsafeAt arr $ unsafeIndex b i
        else Nothing

getNextPos :: Beam -> (Int, Int)
getNextPos (Beam North (r, c)) = (r - 1, c)
getNextPos (Beam East (r, c)) = (r, c + 1)
getNextPos (Beam South (r, c)) = (r + 1, c)
getNextPos (Beam West (r, c)) = (r, c - 1)

continueBeam :: Beam -> Beam
continueBeam beam@(Beam North _) = Beam North (getNextPos beam)
continueBeam beam@(Beam East _) = Beam East (getNextPos beam)
continueBeam beam@(Beam South _) = Beam South (getNextPos beam)
continueBeam beam@(Beam West _) = Beam West (getNextPos beam)

directBeam :: Direction -> Beam -> Beam
directBeam North (Beam _ (r, c)) = Beam North (r - 1, c)
directBeam East (Beam _ (r, c)) = Beam East (r, c + 1)
directBeam South (Beam _ (r, c)) = Beam South (r + 1, c)
directBeam West (Beam _ (r, c)) = Beam West (r, c - 1)

hitMirror :: Char -> Beam -> [Beam]
hitMirror '.' beam = [continueBeam beam]
hitMirror '-' beam@(Beam North (r, c)) = [Beam West (r, c - 1), Beam East (r, c + 1)]
hitMirror '-' beam@(Beam East _) = [continueBeam beam]
hitMirror '-' beam@(Beam South (r, c)) = [Beam West (r, c - 1), Beam East (r, c + 1)]
hitMirror '-' beam@(Beam West _) = [continueBeam beam]
hitMirror '|' beam@(Beam North _) = [continueBeam beam]
hitMirror '|' beam@(Beam East (r, c)) = [Beam North (r - 1, c), Beam South (r + 1, c)]
hitMirror '|' beam@(Beam South _) = [continueBeam beam]
hitMirror '|' beam@(Beam West (r, c)) = [Beam North (r - 1, c), Beam South (r + 1, c)]
hitMirror '\\' beam@(Beam North _) = [directBeam West beam]
hitMirror '\\' beam@(Beam East _) = [directBeam South beam]
hitMirror '\\' beam@(Beam South _) = [directBeam East beam]
hitMirror '\\' beam@(Beam West _) = [directBeam North beam]
hitMirror '/' beam@(Beam North _) = [directBeam East beam]
hitMirror '/' beam@(Beam East _) = [directBeam North beam]
hitMirror '/' beam@(Beam South _) = [directBeam West beam]
hitMirror '/' beam@(Beam West _) = [directBeam South beam]

process :: [Beam] -> RWS (GridUA Char, Set (Int, Int)) () (Set Beam) ()
process [] = return ()
process beams = do
  (grid, is) <- ask
  visited <- get
  let beams' = filter (`S.notMember` visited) beams
  mapM_ (modify . S.insert) beams'
  let nextBeams = filter (\(Beam _ pos) -> S.member pos is) $ concat $ mapMaybe (\b@(Beam _ pos) -> flip hitMirror b <$> grid !? pos) beams'
  process nextBeams

main' :: IO ()
main' = do
  contents <- readFile "2023/inputs/Day16/input.txt"
  let grid = parseGridAsIs contents :: UArray (Int, Int) Char
  let is = S.fromList $ indices grid
  -- part 1
  let (s, _) = execRWS (process [Beam East (0, 0)]) (grid, is) S.empty
  print $ S.size $ S.map (\(Beam _ pos) -> pos) s

  -- part 2
  let (maxR, maxC) = snd $ bounds grid
  let topEntries = [Beam South (0, c) | c <- [0 .. maxC]]
  let leftEntries = [Beam East (r, 0) | r <- [0 .. maxR]]
  let bottomEntries = [Beam North (maxR, c) | c <- [0 .. maxC]]
  let rightEntries = [Beam West (r, maxC) | r <- [0 .. maxR]]
  let entries = topEntries <> leftEntries <> bottomEntries <> rightEntries

  let s2 = map (\b -> execRWS (process [b]) (grid, is) S.empty) entries
  let r2 = map (S.size . S.map (\(Beam _ pos) -> pos) . fst) s2
  print $ maximum r2
  return ()
