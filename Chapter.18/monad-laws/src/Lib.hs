module Lib
  ( Nope(NopeDotJpg)
  , PhbtEither(Left, Right)
  , Identity(Identity)
  , List(Cons, Nil)
  , take'
  , repeat'
  ) where

import Prelude hiding (Either, Left, Right)

-- Nope
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

-- PhbtEither
data PhbtEither b a
  = Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Left x) = Left (f x)
  fmap _ (Right x) = Right x

instance Applicative (PhbtEither b) where
  pure = Left
  (<*>) (Right x) _ = Right x
  (<*>) (Left f) (Right x) = Right x
  (<*>) (Left f) (Left x) = Left (f x)

instance Monad (PhbtEither b) where
  return = pure
  (>>=) (Right x) _ = Right x
  (>>=) (Left x) f = f x

-- Identity
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

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

instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = concat' (f x) (xs >>= f)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' num (Cons x xs) = Cons x $ take' (num - 1) xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

concat' :: List a -> List a -> List a
concat' Nil ys = ys
concat' (Cons x xs) ys = Cons x (concat' xs ys)
