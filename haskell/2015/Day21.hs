module Day21 where

import Data.List
import Data.Maybe
import Data.Tuple.Select

data Item = Item {name :: String, cost, damage', armour' :: Int} deriving (Show, Eq)

data Character = Character {hp, damage, armour :: Int, items :: [Item]} deriving (Show, Eq)

parseItem :: String -> Item
parseItem s = Item n (read c) (read d) (read a)
  where
    [n, c, d, a] = words s

doDamage :: Character -> Character -> Int
doDamage a b = max 1 (damage a - armour b)

viable :: Character -> Character -> Bool
viable p b = p `doDamage` b >= b `doDamage` p

applyItems :: Character -> [Item] -> Character
applyItems c items = c {damage = damage c + (sum . map damage') items, armour = armour c + (sum . map armour') items, items = items}

main' :: IO ()
main' = do
  weapons <- map parseItem . lines <$> readFile "../inputs/2015/Day21/weapons.txt"
  armour <- ([Item "" 0 0 0] <>) . (map parseItem . lines) <$> readFile "../inputs/2015/Day21/armour.txt"
  rings <- ([Item "" 0 0 0, Item "" 0 0 0] <>) . map parseItem . lines <$> readFile "../inputs/2015/Day21/rings.txt"
  let player = Character 100 0 0 []
  let boss = Character 100 8 2 []
  let purchaseCombos = nub $ [[w, a, r1, r2] | w <- weapons, a <- armour, r1 <- rings, r2 <- rings, r1 /= r2]
  -- mapM_ print purchaseCombos
  print $ minimum $ map (sum . map cost) $ filter (flip viable boss . applyItems player) purchaseCombos
  print $ maximum $ map (sum . map cost) $ filter (not . flip viable boss . applyItems player) purchaseCombos
