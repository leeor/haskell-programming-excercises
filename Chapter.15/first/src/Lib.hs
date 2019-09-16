module Lib where

import Test.QuickCheck

newtype First' a = First'
  { getFirst' :: Maybe a
  } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    return (First' x)

instance Monoid (First' a) where
  mempty = First' Nothing
  mappend (First' (Just x)) _ = First' (Just x)
  mappend x y = y
