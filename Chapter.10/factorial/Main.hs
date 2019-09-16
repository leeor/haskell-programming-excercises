module Main where

factorialX :: Integer -> Integer
factorialX 0 = 1
factorialX n = n * factorialX (n - 1)

factorial :: [Integer]
factorial = scanl (*) 1 [1 ..]

main :: IO ()
main = do
  print $ map factorialX [0 .. 10]
  print $ take 5 factorial
  print $ factorial !! 10
