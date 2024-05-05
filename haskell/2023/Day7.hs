module Day7 where

import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.Split
import Data.Tuple.Common

data HandType = HighCard | Pair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord, Enum)

data Hand a = Hand {hand :: [a], hType :: HandType, bid :: Int}
  deriving (Show, Eq)

instance (Ord a) => Ord (Hand a) where
  l `compare` r
    | hType l < hType r = LT
    | hType l > hType r = GT
    | otherwise = go (hand l) (hand r)
    where
      go [] [] = EQ
      go (l : ls) (r : rs) = let o = l `compare` r in if o == EQ then go ls rs else o

newtype Suit = Suit {getSuit :: Char}
  deriving (Eq)

instance Show Suit where
  show s = [getSuit s]

instance Enum Suit where
  toEnum 10 = Suit 'T'
  toEnum 11 = Suit 'J'
  toEnum 12 = Suit 'Q'
  toEnum 13 = Suit 'K'
  toEnum 14 = Suit 'A'
  toEnum c = Suit (intToDigit c)
  fromEnum (Suit 'T') = 10
  fromEnum (Suit 'J') = 11
  fromEnum (Suit 'Q') = 12
  fromEnum (Suit 'K') = 13
  fromEnum (Suit 'A') = 14
  fromEnum (Suit c) = digitToInt c

instance Ord Suit where
  l `compare` r = fromEnum l `compare` fromEnum r

newtype Wild = Wild {getWild :: Char}
  deriving (Eq)

instance Show Wild where
  show s = [getWild s]

instance Enum Wild where
  toEnum 0 = Wild 'J'
  toEnum 10 = Wild 'T'
  toEnum 12 = Wild 'Q'
  toEnum 13 = Wild 'K'
  toEnum 14 = Wild 'A'
  toEnum c = Wild (intToDigit c)
  fromEnum (Wild 'J') = 0
  fromEnum (Wild 'T') = 10
  fromEnum (Wild 'Q') = 12
  fromEnum (Wild 'K') = 13
  fromEnum (Wild 'A') = 14
  fromEnum (Wild c) = digitToInt c

instance Ord Wild where
  l `compare` r = fromEnum l `compare` fromEnum r

determineHType :: String -> HandType
determineHType s = go l
  where
    g = (group . sort) s
    l = length g
    fourOrFull g' = if any ((== 4) . length) g' then FourOfAKind else FullHouse
    threeOrTwo g' = if any ((== 3) . length) g' then ThreeOfAKind else TwoPair
    go l' = case l' of
      1 -> FiveOfAKind
      2 -> fourOrFull g
      3 -> threeOrTwo g
      4 -> Pair
      _ -> HighCard

determineHTypeWild :: String -> HandType
determineHTypeWild s = go l
  where
    (s', js) = partition (/= 'J') s
    extendLastGroup [] = [js]
    extendLastGroup xss = let (xs : xss') = reverse $ sortOn length xss in reverse ((replicate (length js) (head xs) <> xs) : xss')
    g = extendLastGroup $ (group . sort) s'
    l = length g
    fourOrFull g' = if any ((== 4) . length) g' then FourOfAKind else FullHouse
    threeOrTwo g' = if any ((== 3) . length) g' then ThreeOfAKind else TwoPair
    go l' = case l' of
      1 -> FiveOfAKind
      2 -> fourOrFull g
      3 -> threeOrTwo g
      4 -> Pair
      _ -> HighCard

parse :: (String -> HandType) -> String -> Hand Char
parse dh s = Hand {hand = hand, hType = dh hand, bid = bid}
  where
    (hand, bid) = (second read . toTuple . splitOn " ") s :: (String, Int)
    groups = (group . sort) hand

main' :: IO ()
main' = do
  contents <- lines <$> readFile "2023/inputs/Day7/input.txt"
  -- part 1
  let part1 = map ((\h -> h {hand = map Suit (hand h)}) . parse determineHType) contents :: [Hand Suit]
  let sorted1 = sort part1
  print $ sum $ zipWith (\rank hand -> rank * bid hand) [1 ..] sorted1
  -- part 2
  let part2 = map ((\h -> h {hand = map Wild (hand h)}) . parse determineHTypeWild) contents
  let sorted2 = sort part2
  mapM_ print sorted2
  print $ sum $ zipWith (\rank hand -> rank * bid hand) [1 ..] sorted2
