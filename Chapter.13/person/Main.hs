module Main where

import System.IO

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter name: "
  name <- getLine
  putStr "Enter age: "
  age <- getLine
  case mkPerson name (read age) of
    (Right person) -> putStrLn $ "Person: " ++ show person
    (Left err) ->
      case err of
        NameEmpty -> putStrLn "Empty name"
        AgeTooLow -> putStrLn "Age low"
        (PersonInvalidUnknown reason) -> putStrLn reason

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  gimmePerson
