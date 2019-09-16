module Main where

import Data.Char
import Prelude hiding (zip, zipWith)

-- Zip exercises
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (\x y -> (x, y))

-- Data.Char
filterUpper :: String -> String
filterUpper [] = []
filterUpper (x:xs)
  | isUpper x = x : filterUpper xs
  | otherwise = filterUpper xs

capitaliseFirst :: String -> String
capitaliseFirst s@(x:xs)
  | isUpper x = s
  | otherwise = toUpper x : xs

capitaliseAll :: String -> String
capitaliseAll [] = []
capitaliseAll (x:xs)
  | isUpper x = x : capitaliseAll xs
  | otherwise = toUpper x : capitaliseAll xs

capitaliseHead :: String -> Char
capitaliseHead = toUpper . head

main :: IO ()
main = do
  print $ zip [1 .. 10] [2 .. 12]
  print $ zipWith (\x y -> (x, y)) [1 .. 10] [2 .. 12]
  print $ zip' [1 .. 10] [2 .. 12]
  print $ filterUpper "HbEfLrLxO"
  print $ capitaliseFirst "hello"
  print $ capitaliseAll "hello"
  print $ capitaliseHead "hello"
