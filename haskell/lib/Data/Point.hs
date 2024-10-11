module Data.Point where

import Data.Hashable
import GHC.Generics (Generic)

data Point = Point {px, py :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Hashable Point

instance Semigroup Point where
  (Point rx ry) <> (Point lx ly) = Point (lx + ly) (rx + ry)

instance Monoid Point where
  mempty = Point 0 0

fromTuple :: (Int, Int) -> Point
fromTuple (x, y) = Point x y

instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  abs (Point x y) = Point (abs x) (abs y)
  negate (Point x y) = Point (negate x) (negate y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger x = Point (fromInteger x) 0
  (*) = error "tried to multiply two 2D vectors"

manhattanRadius :: Point -> Int
manhattanRadius (Point x y) = abs x + abs y
