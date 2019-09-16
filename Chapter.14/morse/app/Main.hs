module Main where

import Control.Monad (forever, when)
import Data.Traversable (traverse)
import Morse (morseToChar, stringToMorse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getLine, isEOF)

ifWeAreDone :: IO () -> IO ()
ifWeAreDone exitCode = do
  weAreDone <- isEOF
  when weAreDone exitCode

exitOrGetLine :: IO String
exitOrGetLine = do
  ifWeAreDone exitSuccess
  -- otherwise, proceed.
  getLine

convertToMorse :: IO ()
convertToMorse =
  forever $ do
    line <- exitOrGetLine
    convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str) -> putStrLn (unwords str)
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse =
  forever $ do
    line <- exitOrGetLine
    convertLine line
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn
        "Please specify the first argument as being 'from' or 'to' morse, such as: morse to"
      exitFailure
