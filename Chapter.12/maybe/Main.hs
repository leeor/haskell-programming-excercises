module Main where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' d _ Nothing = d
maybe' _ fn (Just x) = fn x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

fromMaybe' :: a -> Maybe a -> a
fromMaybe' d = maybe' d id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms =
  if hasNothing
    then Nothing
    else Just (catMaybes ms)
  where
    hasNothing :: Bool
    hasNothing = foldl (\a b -> a || isNothing b) False ms

main :: IO ()
main = undefined
