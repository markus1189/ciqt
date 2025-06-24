{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Ciqt.Utils (expandTilde, parseNestedJson)
import Data.Aeson qualified as Aeson
import System.Process (readProcessWithExitCode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CIQT Tests"
  [ unitTests
  , goldenTests
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

goldenTests :: TestTree
goldenTests = testGroup "Golden Tests"
  [ goldenVsString 
      "main-help-output" 
      "test/golden/main-help.golden"
      (getCiqtHelpOutput ["--help"])
      
  , goldenVsString
      "run-help-output"
      "test/golden/run-help.golden"
      (getCiqtHelpOutput ["run", "--help"])
      
  , goldenVsString
      "library-help-output"
      "test/golden/library-help.golden"
      (getCiqtHelpOutput ["library", "--help"])
  ]

-- | Helper function to run ciqt command and capture its output
getCiqtHelpOutput :: [String] -> IO LBS.ByteString
getCiqtHelpOutput args = do
  -- Use cabal run to execute the binary with help args
  (_, stdout, stderr) <- readProcessWithExitCode "cabal" (["run", "ciqt", "--"] ++ args) ""
  -- Help output typically goes to stderr, regardless of exit code
  let output = if null stderr then stdout else stderr
  pure $ LBS8.pack output