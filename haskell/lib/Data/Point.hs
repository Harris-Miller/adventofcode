module Data.Point where

data Point a = Point a a
  deriving (Show, Eq, Ord)

instance Functor Point where
  fmap f (Point a b) = Point (f a) (f b)

instance (Num a) => Semigroup (Point a) where
  (Point rx ry) <> (Point lx ly) = Point (lx + ly) (rx + ry)

instance (Num a) => Monoid (Point a) where
  mempty = Point 0 0

fromTuple :: (a, a) -> Point a
fromTuple (x, y) = Point x y

data Point3 a = Point3 a a a
  deriving (Show, Eq)

instance Functor Point3 where
  fmap f (Point3 a b c) = Point3 (f a) (f b) (f c)

instance (Num a) => Semigroup (Point3 a) where
  (Point3 rx ry rz) <> (Point3 lx ly lz) = Point3 (rx + lx) (ry + ly) (rz + lz)

instance (Num a) => Monoid (Point3 a) where
  mempty = Point3 0 0 0

fromTuple3 :: (a, a, a) -> Point3 a
fromTuple3 (x, y, z) = Point3 x y z
