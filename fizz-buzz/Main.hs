module Main where

data FizzBuzz =
  FizzBuzz Int
           Int
           Int
  deriving (Eq, Ord)

instance Show FizzBuzz where
  show (FizzBuzz x y z)
    | fizz && buzz = "FizzBuzz"
    | fizz = "Fizz"
    | buzz = "Buzz"
    | otherwise = show z
    where
      fizz = (mod z x) == 0
      buzz = (mod z y) == 0

fizzBuzz35 = FizzBuzz 3 5

main :: IO ()
main = do
  print $ map (\x -> fizzBuzz35 x) [1 .. 100]
