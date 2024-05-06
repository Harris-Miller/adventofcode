module Data.Tuple.Common where

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)

toTuple3 :: [a] -> (a, a, a)
toTuple3 [a, b, c] = (a, b, c)

apTuple :: (a -> b) -> (a -> c) -> a -> (b, c)
apTuple f1 f2 a = (f1 a, f2 a)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)

tmap3 :: (a -> b) -> (a, a, a) -> (b, b, b)
tmap3 f (a, b, c) = (f a, f b, f c)

toList :: (a, a) -> [a]
toList (a, b) = [a, b]
