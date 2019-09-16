module Main where

foldBoolGuards :: a -> a -> Bool -> a
foldBoolGuards x y flag
  | flag = y
  | otherwise = x

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y flag = case flag of
  True -> y
  False -> x

g :: (a -> b) -> (a, c) -> (b, c)
g fn (a, c) = (fn a, c)

main :: IO ()
main = undefined
