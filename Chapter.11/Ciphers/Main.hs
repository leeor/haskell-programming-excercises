module Main where

import Data.Char

data CipherMode
  = Encipher
  | Decipher
  deriving (Eq, Show)

_vigenere :: CipherMode -> Char -> Char -> Char
_vigenere mode key = shift (ord 'A') 26
  where
    offset =
      if mode == Encipher
        then (+)
        else flip (-)
    shift :: Int -> Int -> Char -> Char
    shift fix span =
      chr .
      (+ fix) . flip mod span . offset (ord key - fix) . flip (-) fix . ord

vigenere :: String -> CipherMode -> String -> String
vigenere _ _ [] = []
vigenere key mode (' ':xs) = ' ' : vigenere key mode xs
vigenere (k:ks) mode (x:xs) = _vigenere mode k x : vigenere (ks ++ [k]) mode xs

testVigenere :: CipherMode -> String -> String
testVigenere = vigenere "ALLY"

main :: IO ()
main = do
  print $ testVigenere Encipher "MEET AT DAWN"
  print $ testVigenere Decipher "MPPR AE OYWY"
