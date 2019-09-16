module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a, Ord a) => a -> a -> a
multiplyBy _ 0 = 0
multiplyBy x y =
  go x (reduceMultiplier y) *
  (if y > 0
     then 1
     else (-1))
  where
    go x' y'
      | y' == 0 = x'
      | otherwise = go (x' + x) (reduceMultiplier y')
    reduceMultiplier m =
      if m > 0
        then m - 1
        else m + 1

main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ (1 + 1) > (1 :: Int) `shouldBe` True
      it "2 + 2 is equal to 4" $ (2 + 2) `shouldBe` (4 :: Int)
      it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)
    describe "Division" $ do
      it "15 divided by 3 is 5" $ dividedBy 15 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2" $ dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiplication" $ do
      it "5 multiplied by 3 is 15" $ multiplyBy 5 3 `shouldBe` (15 :: Int)
      it "5 multiplied by -4 is -20" $
        multiplyBy 5 (-4) `shouldBe` ((-20) :: Int)
      it "-5 multiplied by -4 is 20" $
        multiplyBy (-5) (-4) `shouldBe` (20 :: Int)
      it "9 multiplied by 0 is 0" $ multiplyBy 9 0 `shouldBe` (0 :: Int)
