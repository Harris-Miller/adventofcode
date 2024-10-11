module Data.Point3 where

import Data.Hashable
import GHC.Generics (Generic)

data Point3 = Point3 {px, py, pz :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Semigroup Point3 where
  (Point3 rx ry rz) <> (Point3 lx ly lz) = Point3 (rx + lx) (ry + ly) (rz + lz)

instance Monoid Point3 where
  mempty = Point3 0 0 0

fromTuple3 :: (Int, Int, Int) -> Point3
fromTuple3 (x, y, z) = Point3 x y z
