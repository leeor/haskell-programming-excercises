module Main where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four w x y z)

data Four' a b c =
  Four' a
        b
        c
        c
  deriving (Eq, Show)

instance Functor (Four' a b) where
  fmap f (Four' w x y z) = Four' w x (f y) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Four' a b c) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Four' w x y z)

data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    return (Yeppers x)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

instance Arbitrary b => Arbitrary (Sum a b) where
  arbitrary = Second <$> arbitrary

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor x) = Bloor (f x)
  fmap _ (Desk a) = Desk a
  fmap _ Finance = Finance

instance Arbitrary b => Arbitrary (Quant a b) where
  arbitrary = fmap Bloor arbitrary

newtype K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = fmap K arbitrary

newtype EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = fmap GoatyConst arbitrary

newtype LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

data Parappa f g a =
  DaWrappa (f a)
           (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fx gx) = DaWrappa (fmap f fx) (fmap f gx)

data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fx gx) = IgnoringSomething fx (fmap f gx)

data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious fo fa ft) = Notorious fo fa (fmap f ft)

data List a
  = Nil
  | Cons a
         (List a)

instance Functor List where
  fmap f (Cons x list) = Cons (f x) (fmap f list)
  fmap _ Nil = Nil

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)
  deriving (Show)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read fx) = Read (f . fx)

instance Show (a -> b) where
  show _ = ""

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary =
    oneof
      [ return Halt
      , liftM2 Print arbitrary arbitrary
      , fmap Read (liftArbitrary arbitrary)
      ]

-- OR...
--  arbitrary = do
--    n <- choose (0, 2) :: Gen Int
--    case n of
--      0 -> return Halt
--      1 -> do
--        x <- arbitrary
--        y <- arbitrary
--        return (Print x y)
--      2 -> fmap Read (liftArbitrary arbitrary)
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap g . fmap f $ x) == (fmap (g . f) x)

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck
    (functorCompose :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck
    (functorCompose :: Two Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck
    (functorCompose :: Three Int Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  quickCheck
    (functorCompose :: Three' Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck
    (functorCompose :: Four Int Int Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Four' Int Int Int -> Bool)
  quickCheck
    (functorCompose :: Four' Int Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Possibly Int -> Bool)
  quickCheck
    (functorCompose :: Possibly Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Sum Int Int -> Bool)
  quickCheck
    (functorCompose :: Sum Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: Quant Int Int -> Bool)
  quickCheck
    (functorCompose :: Quant Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: K Int Int -> Bool)
  quickCheck (functorCompose :: K Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: EvilGoateeConst Int Int -> Bool)
  quickCheck
    (functorCompose :: EvilGoateeConst Int Int -> Fun Int Int -> Fun Int Int -> Bool)
  quickCheck (functorIdentity :: TalkToMe Int -> Bool)
  quickCheck
    (functorCompose :: TalkToMe Int -> Fun Int Int -> Fun Int Int -> Bool)
