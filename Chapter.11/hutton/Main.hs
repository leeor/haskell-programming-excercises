module Main where

data Expr
  = Lit Integer
  | Add Expr
        Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

a1 :: Expr
a1 = Add (Lit 9001) (Lit 1)

a2 :: Expr
a2 = Add a1 (Lit 20001)

a3 :: Expr
a3 = Add (Lit 1) a2

main :: IO ()
main = do
  print $ eval (Add (Lit 1) (Lit 9001))
  print $ printExpr (Add (Lit 1) (Lit 9001))
  print $ printExpr a3
