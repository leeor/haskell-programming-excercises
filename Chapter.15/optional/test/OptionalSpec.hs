module Main where

import Test.Hspec
import Test.QuickCheck

import Data.Monoid

import Optional

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Optional Monoid" $
  it "should mappend its contents" $ do
    Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum 2)
    Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product 8)
    Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum 1)
    Only [1] `mappend` Nada `shouldBe` Only [1]
