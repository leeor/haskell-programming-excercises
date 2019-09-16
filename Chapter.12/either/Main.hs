module Main where

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (e:es) = left e ++ lefts' es
  where
    left (Right _) = []
    left (Left a) = [a]

rights' :: [Either a b] -> [b]
rights' [] = []
rights' (e:es) = right e ++ rights' es
  where
    right (Right b) = [b]
    right (Left _) = []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldl partition ([], [])
  where
    partition :: ([a], [b]) -> Either a b -> ([a], [b])
    partition (lefties, righties) (Left a) = (a : lefties, righties)
    partition (lefties, righties) (Right b) = (lefties, b : righties)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _) = Nothing
eitherMaybe f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lFn _ (Left a) = lFn a
either' _ rFn (Right b) = rFn b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

main :: IO ()
main = do
  print $ eitherMaybe'' id (Left '2' :: Either Char Integer)
  print $ eitherMaybe'' id (Right 3 :: Either Char Integer)
