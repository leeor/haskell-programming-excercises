module Main where

import Data.Maybe

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just (Succ (fromJust (integerToNat (x - 1))))

main :: IO ()
main = do
  print $ natToInteger Zero
  print $ natToInteger (Succ Zero)
  print $ natToInteger (Succ (Succ Zero))
  print $ integerToNat 0
  print $ integerToNat 1
  print $ integerToNat 2
  print $ integerToNat (-1)
