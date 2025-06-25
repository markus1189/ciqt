{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck
import Test.Tasty.QuickCheck
import Ciqt.Utils (expandTilde, parseNestedJson)
import Ciqt.History (generateHistoryId, findHistoryEntry)
import Ciqt.Types (HistoryEntry(..), ExecutionStatus(..), LogGroupsArg(..), TimeRange(..), Limit(..))
import Data.Aeson qualified as Aeson
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CIQT Tests"
  [ unitTests
  , historyTests
  , propertyTests
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

historyTests :: TestTree
historyTests = testGroup "History Tests"
  [ testCase "hash generation consistency" $ do
      let query1 = "fields @timestamp | limit 10"
          logGroups1 = CommaLogGroups ["group1", "group2"]
          timeRange1 = RelativeTimeRange "PT1H"
          limit1 = Just (Limit 10)
      
      hash1 <- generateHistoryId query1 logGroups1 timeRange1 limit1 Nothing
      hash2 <- generateHistoryId query1 logGroups1 timeRange1 limit1 Nothing
      
      hash1 @?= hash2
      T.length hash1 @?= 8
      
  , testCase "hash uniqueness for different inputs" $ do
      let query1 = "fields @timestamp | limit 10"
          query2 = "fields @timestamp | limit 20"
          logGroups = CommaLogGroups ["group1"]
          timeRange = RelativeTimeRange "PT1H"
          limit1 = Just (Limit 10)
          limit2 = Just (Limit 20)
      
      hash1 <- generateHistoryId query1 logGroups timeRange limit1 Nothing
      hash2 <- generateHistoryId query2 logGroups timeRange limit2 Nothing
      
      hash1 @/= hash2
      
  , testCase "partial hash matching" $ do
      withSystemTempDirectory "ciqt-test" $ \tempDir -> do
        let historyDir = tempDir </> "history"
        createDirectoryIfMissing True historyDir
        
        -- Create a mock history entry file
        let fullHash = "a1b2c3d4"
            historyFile = historyDir </> (fullHash ++ ".json")
        
        now <- getCurrentTime
        let entry = HistoryEntry
              { _historyId = T.pack fullHash
              , _historyTimestamp = now
              , _historyQuery = "test query"
              , _historyLogGroups = CommaLogGroups ["test"]
              , _historyTimeRange = RelativeTimeRange "PT1H"
              , _historyLimit = Nothing
              , _historyQueryLibrary = Nothing
              , _historyExecutionTime = Nothing
              , _historyStatus = Success
              }
        
        writeFile historyFile (show $ Aeson.toJSON entry)
        
        -- Test partial hash matching
        result1 <- findHistoryEntry historyDir "a1b2"
        result1 @?= Just fullHash
        
        result2 <- findHistoryEntry historyDir "xyz"
        result2 @?= Nothing
        
  , testCase "directory operations" $ do
      withSystemTempDirectory "ciqt-test" $ \tempDir -> do
        let historyDir = tempDir </> "history"
        
        -- Directory should not exist initially
        exists1 <- doesDirectoryExist historyDir
        exists1 @?= False
        
        -- Create directory
        createDirectoryIfMissing True historyDir
        exists2 <- doesDirectoryExist historyDir
        exists2 @?= True
        
        -- Cleanup
        removeDirectoryRecursive historyDir
        exists3 <- doesDirectoryExist historyDir
        exists3 @?= False
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ testProperty "HistoryEntry JSON roundtrip" $ \query logGroupText timeRangeName limitVal -> do
      now <- getCurrentTime
      let entry = HistoryEntry
            { _historyId = "testid01"
            , _historyTimestamp = now
            , _historyQuery = T.pack query
            , _historyLogGroups = CommaLogGroups [T.pack logGroupText]
            , _historyTimeRange = RelativeTimeRange (T.pack timeRangeName)
            , _historyLimit = if limitVal > 0 then Just (Limit $ fromInteger limitVal) else Nothing
            , _historyQueryLibrary = Nothing
            , _historyExecutionTime = Nothing
            , _historyStatus = Success
            }
      let encoded = Aeson.encode entry
          decoded = Aeson.decode encoded
      return $ decoded == Just entry
      
  , testProperty "hash generation produces valid length" $ \query logGroupText timeRangeName -> do
      hash <- generateHistoryId 
        (T.pack query) 
        (CommaLogGroups [T.pack logGroupText])
        (RelativeTimeRange (T.pack timeRangeName))
        Nothing
        Nothing
      return $ T.length hash == 8 && T.all (\c -> c `elem` ("0123456789abcdef" :: String)) hash
  ]