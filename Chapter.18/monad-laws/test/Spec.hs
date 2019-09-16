import Prelude hiding (Either, Left, Right)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid

import Lib

-- Nope
instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- PhbtEither
instance (Arbitrary b, Arbitrary a) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [Left b, Right a]

instance (Eq b, Eq a) => EqProp (PhbtEither b a) where
  (=-=) = eq

-- Identity
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (Identity x) =-= (Identity y) = x `eq` y

-- List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = do
        x <- arbitrary
        xs <- go (n - 1)
        return (Cons x xs)

instance Eq a => EqProp (List a) where
  Nil =-= Nil = True `eq` True
  (Cons x xs) =-= (Cons y ys) = (x `eq` y) .&. (xs' =-= ys')
    where
      xs' = take' 3000 xs
      ys' = take' 3000 ys
  _ =-= _ = False `eq` True

main :: IO ()
main = do
  quickBatch
    ( "Nope"
    , unbatch (functor (undefined :: Nope (Int, String, Int))) <>
      unbatch (applicative (undefined :: Nope (Int, String, Int))) <>
      unbatch (monad (undefined :: Nope (Int, String, Int))))
  quickBatch
    ( "PhbtEither"
    , unbatch
        (functor (undefined :: PhbtEither (Int, String, Int) (Int, String, Int))) <>
      unbatch
        (applicative
           (undefined :: PhbtEither (Int, String, Int) (Int, String, Int))) <>
      unbatch
        (monad (undefined :: PhbtEither (Int, String, Int) (Int, String, Int))))
  quickBatch
    ( "Identity"
    , unbatch (functor (undefined :: Identity (Int, String, Int))) <>
      unbatch (applicative (undefined :: Identity (Int, String, Int))) <>
      unbatch (monad (undefined :: Identity (Int, String, Int))))
  quickBatch
    ( "List"
    , unbatch (functor (undefined :: List (Int, String, Int))) <>
      unbatch (applicative (undefined :: List (Int, String, Int))) <>
      unbatch (monad (undefined :: List (Int, String, Int))))
