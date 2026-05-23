module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import UnitTests (unitTests)

main :: IO ()
main = defaultMain (testGroup "TOA" [unitTests])
