{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Test.QuickCheck

data Bool'
  = True'
  | False'
  deriving (Generic)

instance CoArbitrary Bool'

-- plus the above
trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

main :: IO ()
main = do
  sample falseGen
