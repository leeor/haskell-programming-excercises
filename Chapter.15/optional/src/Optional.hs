module Optional where

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = undefined
  mappend (Only x) (Only y) = Only $ mappend x y
  mappend Nada Nada = Nada
  mappend x Nada = x
  mappend Nada x = x
