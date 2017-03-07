module Main where

import qualified Test.Property as Property
import           Test.Tasty
import qualified Test.Unit     as Unit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Property.tests, Unit.tests]
