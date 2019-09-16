module Main where

import qualified Data.Monoid as M
import Data.Semigroup
import Test.QuickCheck

-- Trivial
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type TrivialLeftIdentity = Trivial -> Bool

type TrivialRightIdentity = Trivial -> Bool

-- Identity
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity $ mappend x y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

type IdentityLeftIdentity a = Identity a -> Bool

type IdentityRightIdentity a = Identity a -> Bool

-- Two
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- BoolConj
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj x) (BoolConj y) = BoolConj $ x && y

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- BoolDisj
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj x) (BoolDisj y) = BoolDisj $ x || y

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Or
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst x) <> (Fst y) = Fst y
  (Fst x) <> (Snd y) = Snd y
  (Snd x) <> _ = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return (Fst x)), (1, return (Snd y))]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- Combine
newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (Combine x) <> (Combine y) = Combine $ \n -> x n <> y n

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend (Combine x) (Combine y) = Combine $ \n -> mappend (x n) (y n)

-- Comp
newtype Comp a = Comp
  { unComp :: a -> a
  }

instance Semigroup a => Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp $ x . y

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend (Comp x) (Comp y) = Comp $ x . y

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

semigroupAssocCombine :: (Eq m, Semigroup m, Num m) => m -> Bool
semigroupAssocCombine a =
  unCombine ((Combine (1 +) <> (Combine $ \n -> n - 1)) <> Combine (1 +)) a ==
  unCombine (Combine (1 +) <> ((Combine $ \n -> n - 1) <> Combine (1 +))) a

monoidLeftIdentityCombine :: (Eq m, Monoid m, Num m) => m -> Bool
monoidLeftIdentityCombine a = unCombine (mappend f mempty) a == unCombine f a
  where
    f :: Num a => Combine a (Sum a)
    f = Combine $ \n -> Sum (n + 1)

monoidRightIdentityCombine :: (Eq m, Monoid m, Num m) => m -> Bool
monoidRightIdentityCombine a = unCombine (mappend mempty f) a == unCombine f a
  where
    f :: Num a => Combine a (Sum a)
    f = Combine $ \n -> Sum (n + 1)

semigroupAssocComp :: (Eq m, Semigroup m, Num m) => m -> Bool
semigroupAssocComp a =
  unComp ((Comp (1 +) <> (Comp $ \n -> n - 1)) <> Comp (1 +)) a ==
  unComp (Comp (1 +) <> ((Comp $ \n -> n - 1) <> Comp (1 +))) a

-- Validation
data Validation a b
  = Failur a
  | Succes b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Succes x) <> (Succes y) = Succes x
  (Succes x) <> (Failur y) = Succes x
  (Failur x) <> (Succes y) = Succes y
  (Failur x) <> (Failur y) = Failur (x <> y)

checkValidation :: IO ()
checkValidation = do
  let failure :: String -> Validation String Int
      failure = Failur
      success :: Int -> Validation String Int
      success = Succes
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mappend mempty a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mappend a mempty == a

newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \n -> (mempty, n)
  mappend (Mem x) (Mem y) =
    Mem $ \n ->
      let x' = x n
          y' = y (snd x')
      in (fst x' M.<> fst y', snd y')

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String String)
  quickCheck (semigroupAssocCombine :: Sum Int -> Bool)
  quickCheck (semigroupAssocComp :: Sum Int -> Bool)
  checkValidation
  quickCheck (monoidLeftIdentity :: TrivialLeftIdentity)
  quickCheck (monoidRightIdentity :: TrivialRightIdentity)
  quickCheck (monoidLeftIdentity :: IdentityLeftIdentity String)
  quickCheck (monoidRightIdentity :: IdentityRightIdentity String)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidLeftIdentityCombine :: Sum Int -> Bool)
  quickCheck (monoidRightIdentityCombine :: Sum Int -> Bool)
  let rmzero = runMem mempty 0
      rmleft = runMem (f' M.<> mempty) 0
      rmright = runMem (mempty M.<> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
