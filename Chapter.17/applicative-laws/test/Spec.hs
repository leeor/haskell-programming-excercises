import Test.QuickCheck (Arbitrary, arbitrary, elements, sized)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid

import Lib

-- ZipList'
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = do
        x <- arbitrary
        xs <- go (n - 1)
        return (Cons x xs)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take' 3000 l
      ys' =
        let (ZipList' l) = ys
        in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    x <- arbitrary
    return $ ZipList' x

-- Validation
instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) (Success x) (Success y) = x `eq` y
  (=-=) (Failure x) (Failure y) = x `eq` y
  (=-=) (Success _) (Failure _) = False `eq` True
  (=-=) (Failure _) (Success _) = False `eq` True

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure e, Success a]

-- Pair
instance Eq a => EqProp (Pair a) where
  (Pair x1 y1) =-= (Pair x2 y2) = (x1 `eq` x2) .&. (y1 `eq` y2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

--Two
instance (Eq a, Eq b) => EqProp (Two a b) where
  (Two x1 y1) =-= (Two x2 y2) = (x1 `eq` x2) .&. (y1 `eq` y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

--Three
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (Three x1 y1 z1) =-= (Three x2 y2 z2) =
    (x1 `eq` x2) .&. (y1 `eq` y2) .&. (z1 `eq` z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

--Three'
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (Three' x1 y1 z1) =-= (Three' x2 y2 z2) =
    (x1 `eq` x2) .&. (y1 `eq` y2) .&. (z1 `eq` z2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

--Four
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (Four w1 x1 y1 z1) =-= (Four w2 x2 y2 z2) =
    (w1 `eq` w2) .&. (x1 `eq` x2) .&. (y1 `eq` y2) .&. (z1 `eq` z2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

--Four'
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (Four' w1 x1 y1 z1) =-= (Four' w2 x2 y2 z2) =
    (w1 `eq` w2) .&. (x1 `eq` x2) .&. (y1 `eq` y2) .&. (z1 `eq` z2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)

main :: IO ()
main = do
  quickBatch
    ( "Validation"
    , unbatch (functor (undefined :: Validation String (Int, Double, Char))) <>
      unbatch (applicative (undefined :: Validation String (Int, Double, Char))))
  quickBatch
    ( "ZipList'"
    , unbatch (functor (undefined :: ZipList' (Int, Double, Char))) <>
      unbatch (applicative (undefined :: ZipList' (Int, Double, Char))))
  quickBatch
    ( "Pair"
    , unbatch (functor (undefined :: Pair (Int, Double, Char))) <>
      unbatch (applicative (undefined :: Pair (Int, Double, Char))))
  quickBatch
    ( "Two"
    , unbatch
        (functor (undefined :: Two (Int, Double, Char) (Int, Double, Char))) <>
      unbatch
        (applicative
           (undefined :: Two (Sum Int, Product Int, String) (Int, Double, Char))))
  quickBatch
    ( "Three"
    , unbatch
        (functor
           (undefined :: Three (Int, Double, Char) (Int, Double, Char) ( Int
                                                                       , Double
                                                                       , Char))) <>
      unbatch
        (applicative
           (undefined :: Three (Sum Int, Product Int, String) ( Sum Int
                                                              , Product Int
                                                              , String) ( Int
                                                                        , Double
                                                                        , Char))))
  quickBatch
    ( "Three'"
    , unbatch
        (functor (undefined :: Three' (Int, Double, Char) (Int, Double, Char))) <>
      unbatch
        (applicative
           (undefined :: Three' (Sum Int, Product Int, String) ( Int
                                                               , Double
                                                               , Char))))
  quickBatch
    ( "Four"
    , unbatch
        (functor
           (undefined :: Four (Int, Double, Char) (Int, Double, Char) ( Int
                                                                      , Double
                                                                      , Char) ( Int
                                                                              , Double
                                                                              , Char))) <>
      unbatch
        (applicative
           (undefined :: Four (Sum Int, Product Int, String) ( Sum Int
                                                             , Product Int
                                                             , String) ( Sum Int
                                                                       , Product Int
                                                                       , String) ( Int
                                                                                 , Double
                                                                                 , Char))))
  quickBatch
    ( "Four'"
    , unbatch
        (functor (undefined :: Four' (Int, Double, Char) (Int, Double, Char))) <>
      unbatch
        (applicative
           (undefined :: Four' (Sum Int, Product Int, String) ( Int
                                                              , Double
                                                              , Char))))
