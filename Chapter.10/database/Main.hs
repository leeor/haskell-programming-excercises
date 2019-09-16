module Main where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr collectDates []
  where
    collectDates (DbDate date) b = date : b
    collectDates _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr collectNumbers []
  where
    collectNumbers (DbNumber num) b = num : b
    collectNumbers _ b = b

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = avg . filterDbNumber
  where
    avg :: [Integer] -> Double
    avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

main :: IO ()
main = do
  print $ filterDbDate theDatabase
  print $ filterDbNumber theDatabase
  print $ mostRecent theDatabase
  print $ sumDb theDatabase
  print $ avgDb theDatabase
