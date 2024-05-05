module SearchTests where

import Algorithm.Search

data Tree a = Tree a [Tree a]
  deriving (Show, Eq, Ord)

genBfs :: [String]
genBfs = go ["1"]
  where
    go (x : xs) = x : go (xs <> [n1, n2])
      where
        n1 = x ++ "1"
        n2 = x ++ "2"

tree :: Tree String
tree = Tree "1" [Tree "11" [Tree "111" [], Tree "112" []], Tree "12" [Tree "121" [], Tree "122" []]]

main' :: IO ()
main' = do
  let gen = take 7 genBfs
  print gen

  let next x = [x ++ "1", x ++ "2"]
  let found = (== "122")

  let r1 = bfs next found "1"
  print r1

  let next2 (Tree _ c) = c
  let found2 (Tree v _) = v == "122"

  let r2 = dfs next2 found2 tree
  print r2

  return ()
