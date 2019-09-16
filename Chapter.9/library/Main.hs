module Main where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem needle (x:xs) = needle == x || myElem needle xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' needle haystack = any (\x -> x == needle) haystack

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = go xs [x]
  where
    go [] acc = acc
    go (y:ys) acc = go ys (y : acc)

mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish (x:xs) = x ++ mySquish xs

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap _ [] = []
mySquishMap f (x:xs) = (f x) ++ mySquishMap f xs

mySquish' = mySquishMap (\x -> x)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) =
  case f x y of
    GT -> x
    otherwise -> y
  where
    y = myMaximumBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) =
  case f x y of
    LT -> x
    otherwise -> y
  where
    y = myMinimumBy f xs

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

main = do
  print $ myOr [False, False]
  print $ myOr [False, False, True]
  print $ myAny id [False, False]
  print $ myAny id [False, False, True]
  print $ myAny even [1, 3, 5]
  print $ myAny odd [1, 3, 5]
  print $ myElem 1 [1 .. 10]
  print $ myElem 10 [1 .. 10]
  print $ myElem 1 [2 .. 10]
  print $ myElem' 1 [1 .. 10]
  print $ myElem' 10 [1 .. 10]
  print $ myElem' 1 [2 .. 10]
  print $ myReverse "blah"
  print $ myReverse [1 .. 5]
  print $ mySquish [[1, 2, 3], [4, 5, 6]]
  print $ mySquishMap (\x -> [1 .. x]) [1, 2, 3]
  print $ mySquish' [[1, 2, 3], [4, 5, 6]]
  print $ myMaximumBy compare [1, 53, 9001, 10]
  print $ myMinimumBy compare [1, 53, 9001, 10]
  print $ myMaximum [1, 53, 9001, 10]
  print $ myMinimum [1, 53, 9001, 10]
