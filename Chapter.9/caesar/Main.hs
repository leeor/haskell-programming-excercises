module Main where

import Data.Char

_caesar :: Bool -> Int -> Char -> Char
_caesar mode off x
  | isLower x = shift (ord 'a') 26 x
  | isUpper x = shift (ord 'A') 26 x
  | isDigit x = shift (ord '0') 10 x
  | otherwise = x
  where
    offset :: Int
    offset =
      if mode
        then off
        else (-off)
    shift :: Int -> Int -> Char -> Char
    shift fix span =
      chr . (+ fix) . flip mod span . (+ offset) . flip (-) fix . ord

caesar :: Int -> Char -> Char
caesar = _caesar True

unCaesar :: Int -> Char -> Char
unCaesar = _caesar False

cipher :: (Char -> Char) -> String -> String
cipher _ [] = []
cipher f (x:xs) = f x : cipher f xs

main :: IO ()
main = do
  print $ cipher (caesar 1) "abc0123456789xyz"
  print $ cipher (unCaesar 1) "bcd1234567890yza"
  print $ cipher (caesar 26) "abc0123456789xyz"
  print $ cipher (unCaesar 26) "bcd1234567890yza"
  print $ cipher (caesar 13) "abc0123456789xyz"
  print $ cipher (unCaesar 13) "bcd1234567890yza"
