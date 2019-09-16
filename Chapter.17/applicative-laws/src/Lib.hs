module Lib
  ( List(Nil, Cons)
  , ZipList'(ZipList')
  , Validation(Failure, Success)
  , Pair(Pair)
  , Two(Two)
  , Three(Three)
  , Three'(Three')
  , Four(Four)
  , Four'(Four')
  , take'
  , repeat'
  ) where

import Data.Monoid

-- List
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = go f xs
    where
      go _ Nil = fs <*> xs
      go f (Cons y ys) = Cons (f y) $ go f ys

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' num (Cons x xs) = Cons x $ take' (num - 1) xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

-- ZipList'
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs)) = ZipList' (Cons (f x) l)
    where
      (ZipList' l) = ZipList' fs <*> ZipList' xs

-- Validation
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success x) = Success (f x)
  (<*>) (Failure x) (Failure y) = Failure (x <> y)
  (<*>) _ (Failure x) = Failure x
  (<*>) (Failure x) _ = Failure x

-- Pair
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x y) = Pair (f1 x) (f2 y)

-- Two
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two x1 y1) (Two x2 y2) = Two (x1 `mappend` x2) (y1 y2)

-- Three
data Three a b c =
  Three a
        b
        c
  deriving (Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three x1 y1 z1) (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 z2)

-- Three'
data Three' a b =
  Three' a
         b
         b
  deriving (Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x1 y1 z1) (Three' x2 y2 z2) = Three' (x1 <> x2) (y1 y2) (z1 z2)

-- Four
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four w1 x1 y1 z1) (Four w2 x2 y2 z2) =
    Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 z2)

-- Four'
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' w1 x1 y1 z1) (Four' w2 x2 y2 z2) =
    Four' (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 z2)
