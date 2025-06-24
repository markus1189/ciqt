{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Ciqt.Utils (expandTilde, parseNestedJson)
import Data.Aeson qualified as Aeson

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CIQT Tests"
  [ unitTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testGroup "Utility Tests"
    [ testCase "expandTilde utility" $ do
        result <- expandTilde "/tmp/test"
        result @?= "/tmp/test"
        
    , testCase "parseNestedJson utility" $ do
        let input = Aeson.String "{\"key\": \"value\"}"
            expected = Aeson.object [("key", Aeson.String "value")]
            result = parseNestedJson input
        result @?= expected
    ]
  ]