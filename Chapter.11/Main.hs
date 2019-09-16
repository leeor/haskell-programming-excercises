module Main where

import Data.Char
import Data.List

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf needle@(n:ns) (s:ss)
  | n == s = isSubseqOf ns ss
  | otherwise = isSubseqOf needle ss

capitalizeWord :: String -> String
capitalizeWord (' ':xs) = ' ' : capitalizeWord xs
capitalizeWord word = (toUpper (head word)) : tail word

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w -> (w, capitalizeWord w)) . words

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p = go . capitalizeWord $ p
  where
    go :: String -> String
    go [] = []
    go ('.':xs) = '.' : capitalizeParagraph xs
    go (x:xs) = x : go xs

main :: IO ()
main = do
  print $ isSubseqOf "blah" "blahwoot"
  print $ isSubseqOf "blah" "wootblah"
  print $ isSubseqOf "blah" "wboloath"
  print $ isSubseqOf "blah" "wootbla"
  print $ isSubseqOf "blah" "halbwoot"
  print $ isSubseqOf "blah" "blawhoot"
  print $ capitalizeWords "hello world"
  print $ capitalizeParagraph "blah. woot ha."
