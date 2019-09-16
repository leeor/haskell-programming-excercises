module Main where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f s = go (f s)
  where
    go Nothing = Leaf
    go (Just (l, v, r)) = Node (unfold f l) v (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    (\x ->
       if x >= n
         then Nothing
         else Just (x + 1, x, x + 1))
    0

main :: IO ()
main = do
  print $ treeBuild 0
  print $ treeBuild 1
  print $ treeBuild 2
  print $ treeBuild 3
