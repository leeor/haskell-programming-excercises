module Main where

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe = unwords . map (go . notThe) . words
  where
    go :: Maybe String -> String
    go Nothing = "a"
    go (Just s) = s

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = flip elem vowels

filterVowels :: String -> String
filterVowels = filter isVowel

countVowels :: String -> Int
countVowels = length . filterVowels

countTheBeforVowel :: String -> Integer
countTheBeforVowel = sum . map isBeginningWithVowel . f1 . words
  where
    isBeginningWithVowel :: String -> Integer
    isBeginningWithVowel w
      | isVowel $ head w = 1
      | otherwise = 0
    f1 :: [String] -> [String]
    f1 [] = []
    f1 [_] = []
    f1 (x:xs) = _filter (notThe x)
      where
        _filter :: Maybe String -> [String]
        _filter Nothing = head xs : f1 (tail xs)
        _filter (Just _) = f1 (tail xs)

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w
  | numVowels > numCons = Nothing
  | otherwise = Just (Word' w)
  where
    numVowels = length . filterVowels $ w
    numCons = length w - numVowels

main :: IO ()
main = do
  print $ replaceThe "the cow loves us"
  print $ countTheBeforVowel "the cow"
  print $ countTheBeforVowel "the evil cow"
  print $ countVowels "the cow"
  print $ countVowels "Mikolajczak"
