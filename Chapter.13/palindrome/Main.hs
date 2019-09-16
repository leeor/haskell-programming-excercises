module Main where

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    if normalised line1 == (reverse . normalised) line1
      then do
        putStrLn "It's a palindrome!"
        exitSuccess
      else putStrLn "Nope!"
  where
    normalised = fmap toLower . filter isAlpha

main :: IO ()
main = palindrome
