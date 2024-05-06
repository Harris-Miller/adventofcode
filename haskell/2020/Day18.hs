module Day18 where

data Operation = Add | Mul

data Node a = Value a | Expression a Operation a

main' :: IO ()
main' = do
  return ()
