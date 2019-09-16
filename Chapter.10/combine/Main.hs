module Main where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: [(Char, Char, Char)]
combinations = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]

pCombinations :: [(Char, Char, Char)]
pCombinations = [(s, v, s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

main :: IO ()
main = do
  print $ length combinations
  print $ length stops * length vowels * length stops == length combinations
  print $ length pCombinations
  print $ pCombinations
  print $ length vowels * length stops == length combinations
