module Day17 where

import Data.Grid.Map
import Data.Grid3.Map
import Data.List
import Data.Map qualified as M
import Data.Maybe

type Grid3Cube = Grid3M Cube

data Cube = Active | Inactive
  deriving (Eq, Show)

parseCube :: Char -> Cube
parseCube '.' = Inactive
parseCube '#' = Active

cubeToChar :: Cube -> Char
cubeToChar Inactive = '.'
cubeToChar Active = '#'

createBlankLayer :: (Int, Int) -> Int -> Grid3Cube
createBlankLayer (c, r) l = M.fromList forMap
  where
    list = [(l, c', r') | c' <- [0 .. c], r' <- [0 .. r]]
    forMap = map (,Inactive) list

applyRules :: Grid3Cube -> (Int, Int, Int) -> Cube -> Cube
applyRules cube coord value = if value == Active then whenActive else whenInactive
  where
    neighbors = get3NeighborsCyclical cube coord
    totalActive = length $ filter ((== Active) . snd) neighbors
    whenActive = if totalActive == 2 || totalActive == 3 then Active else Inactive
    whenInactive = if totalActive == 3 then Active else Inactive

main' :: IO ()
main' = do
  content <- readFile "../inputs/2020/Day17/sample.txt"
  let middleLayer = parseGrid (const True) parseCube content
  let maxCube = fst $ M.findMax middleLayer
  let middleLayer3d = M.fromList $ map (\((c, r), v) -> ((0, c, r), v)) $ M.toList middleLayer

  let theCube = middleLayer3d <> createBlankLayer maxCube (-1) <> createBlankLayer maxCube 1

  -- mapM_ (printGrid (singleton . cubeToChar)) $ toLayers theCube

  -- putStrLn $ gridToStr

  putStrLn ""

  let theCube' = M.mapWithKey (applyRules theCube) theCube
  let layers = toLayers theCube'

  -- mapM_ (printGrid (singleton . cubeToChar)) layers

  return ()
