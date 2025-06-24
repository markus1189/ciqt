{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Data.Text qualified as Text
import Ciqt.Utils (expandTilde, parseNestedJson)
import Data.Aeson qualified as Aeson

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CIQT Tests"
  [ unitTests
  , goldenTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testGroup "Hello World Tests"
    [ testCase "Basic arithmetic" $ do
        let result = 2 + 2 :: Int
        result @?= 4
        
    , testCase "Text concatenation" $ do
        let result = Text.append "Hello, " "World!"
        result @?= "Hello, World!"
        
    , testCase "expandTilde utility" $ do
        result <- expandTilde "/tmp/test"
        result @?= "/tmp/test"
        
    , testCase "parseNestedJson utility" $ do
        let input = Aeson.String "{\"key\": \"value\"}"
            expected = Aeson.object [("key", Aeson.String "value")]
            result = parseNestedJson input
        result @?= expected
    ]
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden Tests"
  [ goldenVsString 
      "hello-world-output" 
      "test/golden/hello-world.golden"
      (pure "Hello, World!\n")
      
  , goldenVsString
      "simple-json-output"
      "test/golden/simple-json.golden"
      (pure "{\"message\": \"Hello from CIQT tests!\"}\n")
  ]