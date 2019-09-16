module Main where

import Control.Applicative ()
import Data.Char
import Data.List
import Data.Maybe

type Digit = Char

data Button = Button
  { digit :: Digit
  , chars :: String
  }

button1 :: Button
button1 = Button '1' "1"

button2 :: Button
button2 = Button '2' "abc2"

button3 :: Button
button3 = Button '3' "def3"

button4 :: Button
button4 = Button '4' "ghi4"

button5 :: Button
button5 = Button '5' "jkl5"

button6 :: Button
button6 = Button '6' "mno6"

button7 :: Button
button7 = Button '7' "pqrs7"

button8 :: Button
button8 = Button '8' "tuv8"

button9 :: Button
button9 = Button '9' "wxyz9"

button0 :: Button
button0 = Button '0' "+ "

buttonHash :: Button
buttonHash = Button '#' ".,"

buttonStar :: Button
buttonStar = Button '*' ""

-- validButtons = "1234567890*#"
newtype DaPhone = DaPhone
  { buttons :: [Button]
  }

daPhone :: DaPhone
daPhone =
  DaPhone
    [ button1
    , button2
    , button3
    , button4
    , button5
    , button6
    , button7
    , button8
    , button9
    , button0
    , buttonStar
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- Valid presses: 1 and up
type Presses = Int

charToTaps :: Char -> Button -> Maybe (Digit, Presses)
charToTaps c button = taps <$> elemIndex c (chars button)
  where
    taps :: Presses -> (Digit, Presses)
    taps i = (digit button, i + 1)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone charToType
  | isUpper charToType = ('*', 1) : reverseTaps phone (toLower charToType)
  | otherwise = mapMaybe (charToTaps charToType) (buttons phone)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((_, p):xs) = p + fingerTaps xs

foldCount :: Eq a => a -> [(a, Integer)] -> [(a, Integer)]
foldCount c [] = [(c, 1)]
foldCount c counts@((char, count):cs)
  | c == char = (char, count + 1) : cs
  | otherwise = (c, 1) : counts

countLetters :: String -> [(Char, Integer)]
countLetters = foldr foldCount []

mostPopularLetter :: String -> Char
mostPopularLetter =
  fst . maximumBy (\x y -> compare (snd x) (snd y)) . countLetters . sort

countWords :: [String] -> [(String, Integer)]
countWords = foldr foldCount []

mostPopularWord :: [String] -> String
mostPopularWord =
  fst .
  maximumBy (\x y -> compare (snd x) (snd y)) .
  countWords . sort . words . unwords

costOfLetter :: DaPhone -> Char -> Presses
costOfLetter phone c = fingerTaps $ reverseTaps phone c

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

main :: IO ()
main = do
  print $ reverseTaps daPhone 'a'
  print $ reverseTaps daPhone 'A'
  print $ map (cellPhonesDead daPhone) convo
  print $ map (fingerTaps . cellPhonesDead daPhone) convo
  print $ map mostPopularLetter convo
  print $ map (costOfLetter daPhone . mostPopularLetter) convo
  print $ coolestLtr convo
  print $ mostPopularWord convo
