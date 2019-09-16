module Main where

import Data.Monoid
import Test.QuickCheck

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <> adv <> " as he jumped into his car " <> noun <>
  " and drove off with his " <>
  adj <>
  " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with his "
    , adj
    , " wife."
    ]

madlibEquality :: Exclamation -> Adverb -> Noun -> Adjective -> Bool
madlibEquality e adv noun adj =
  madlibbin' e adv noun adj == madlibbinBetter' e adv noun adj

main :: IO ()
main = quickCheck madlibEquality
