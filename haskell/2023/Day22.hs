module Day22 where

import Control.Arrow
import Control.Monad.State
import Data.Ix
import Data.List
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Point3

type Cube = [Point3]

type CubeMap = Map Char Cube

parse :: String -> Cube
parse s = [Point3 x y z | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]]
  where
    [[x1, y1, z1], [x2, y2, z2]] = (map (map read . splitOn ",") . splitOn "~") s

getZs :: Cube -> [Int]
getZs = nub . map (\(Point3 _ _ z) -> z)

getMinZ :: Cube -> Int
getMinZ = minimum . getZs

getMaxZ :: Cube -> Int
getMaxZ = maximum . getZs

sortByLowest :: [Cube] -> [Cube]
sortByLowest = sortBy (\l r -> getMinZ l `compare` getMinZ r)

shiftDown :: Cube -> Cube
shiftDown = map (\(Point3 x y z) -> Point3 x y (z - 1))

shiftUp :: Cube -> Cube
shiftUp = map (\(Point3 x y z) -> Point3 x y (z + 1))

getAllZPoints :: Int -> Cube -> Cube
getAllZPoints z = filter (\(Point3 _ _ pz) -> pz == z)

mapAllZPoints :: Int -> CubeMap -> Cube
mapAllZPoints z = concatMap (getAllZPoints z) . M.elems

getPlaneBelow :: Cube -> Cube
getPlaneBelow = shiftDown . uncurry getAllZPoints . (getMinZ &&& id)

canMoveInto :: CubeMap -> Cube -> Bool
canMoveInto cm c = (z - 1) /= 0 && not (any (`elem` allAtZ) cubeZs)
  where
    z = getMinZ c
    cubeZs = shiftDown $ getAllZPoints z c
    allAtZ = mapAllZPoints (z - 1) cm

doesSupport :: CubeMap -> Cube -> Bool
doesSupport cm c = any (`elem` allAtZ) cubeZs
  where
    z = getMaxZ c
    cubeZs = shiftUp $ getAllZPoints z c
    allAtZ = mapAllZPoints (z + 1) cm

lowerAll :: [Char] -> State CubeMap ()
lowerAll [] = return ()
lowerAll (k : ks) = do
  cubeMap <- get
  let cube = cubeMap M.! k
  let lowerPlane = getPlaneBelow cube
  let canMove = canMoveInto cubeMap cube
  when canMove (modify (M.insert k (shiftDown cube)) >> lowerAll (k : ks))
  lowerAll ks

main' :: IO ()
main' = do
  contents <- sortByLowest . map parse . lines <$> readFile "../inputs/2023/Day22/sample.txt"
  let cubeMap = M.fromList $ zip ['A' ..] contents :: CubeMap
  let keyOrderList = M.keys cubeMap
  let numBlocks = length keyOrderList

  -- part 1
  let result = execState (lowerAll keyOrderList) cubeMap
  print $ M.map (doesSupport result) result
  print $ numBlocks - length (filter id $ M.elems $ M.map (doesSupport result) result)

  return ()
