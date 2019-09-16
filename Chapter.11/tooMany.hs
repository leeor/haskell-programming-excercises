{-# LANGUAGE FlexibleInstances #-}

module TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, Int) where
  tooMany (n, n') = (n + n') > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42
