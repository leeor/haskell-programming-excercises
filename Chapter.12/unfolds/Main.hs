module Main where

myIterate :: (a -> a) -> a -> [a]
myIterate f s = s : myIterate f (f s)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = go (f s)
  where
    go Nothing = []
    go (Just (x, y)) = x : myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

main :: IO ()
main = do
  print $ take 10 $ myIterate (+ 1) 0
  print $ take 10 $ myUnfoldr (\b -> Just (b, b + 1)) 0
  print $ take 10 $ betterIterate (+ 1) 0
