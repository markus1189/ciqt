{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck
import Test.Tasty.QuickCheck
import Ciqt.Utils (expandTilde, parseNestedJson)
import Ciqt.History (generateHistoryId, findHistoryEntry)
import Ciqt.Types (HistoryEntry(..), ExecutionStatus(..), LogGroupsArg(..), TimeRange(..), Limit(..), RunArgs(..), QueryArg(..))
import qualified Ciqt.Types as CT
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Time (UTCTime, getCurrentTime, CalendarDiffTime(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NE
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
          logGroups1 = CommaLogGroups (NE.fromList ["group1", "group2"])
          timeRange1 = TimeRangeRelative (CalendarDiffTime 0 3600)
          limit1 = Just (ExplicitLimit 10)
      
      let dummyRunArgs = RunArgs (QueryString "test") Nothing Nothing Nothing False Nothing
      now <- getCurrentTime
      let hash1 = generateHistoryId dummyRunArgs now logGroups1 timeRange1
          hash2 = generateHistoryId dummyRunArgs now logGroups1 timeRange1
      
      hash1 @?= hash2
      T.length hash1 @?= 8
      
  , testCase "hash uniqueness for different inputs" $ do
      let query1 = "fields @timestamp | limit 10"
          query2 = "fields @timestamp | limit 20"
          logGroups = CommaLogGroups (NE.fromList ["group1"])
          timeRange = TimeRangeRelative (CalendarDiffTime 0 3600)
          limit1 = Just (ExplicitLimit 10)
          limit2 = Just (ExplicitLimit 20)
      
      let dummyRunArgs1 = RunArgs (QueryString "test1") Nothing Nothing Nothing False Nothing
          dummyRunArgs2 = RunArgs (QueryString "test2") Nothing Nothing Nothing False Nothing
      now <- getCurrentTime
      let hash1 = generateHistoryId dummyRunArgs1 now logGroups timeRange
          hash2 = generateHistoryId dummyRunArgs2 now logGroups timeRange
      
      hash1 /= hash2 @? "Hashes should be different"
      
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
              , _historyLogGroups = CommaLogGroups (NE.fromList ["test"])
              , _historyTimeRange = TimeRangeRelative (CalendarDiffTime 0 3600)
              , _historyLimit = Nothing
              , _historyQueryLibrary = Nothing
              , _historyExecutionTime = Nothing
              , _historyStatus = CT.Success
              }
        
        LBS.writeFile historyFile (Aeson.encode entry)
        
        -- Test partial hash matching
        result1 <- findHistoryEntry (Just historyDir) "a1b2"
        case result1 of
          Just entry -> _historyId entry @?= T.pack fullHash
          Nothing -> assertFailure "Expected to find history entry"
        
        result2 <- findHistoryEntry (Just historyDir) "xyz"
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
  [ testCase "HistoryEntry JSON roundtrip" $ do
      now <- getCurrentTime
      let entry = HistoryEntry
            { _historyId = "testid01"
            , _historyTimestamp = now
            , _historyQuery = "test query"
            , _historyLogGroups = CommaLogGroups (NE.fromList ["test-group"])
            , _historyTimeRange = TimeRangeRelative (CalendarDiffTime 0 3600)
            , _historyLimit = Just (ExplicitLimit 10)
            , _historyQueryLibrary = Nothing
            , _historyExecutionTime = Nothing
            , _historyStatus = CT.Success
            }
      let encoded = Aeson.encode entry
          decoded = Aeson.decode encoded
      decoded @?= Just entry
      
  , testCase "hash generation produces valid length" $ do
      let dummyRunArgs = RunArgs (QueryString "test") Nothing Nothing Nothing False Nothing
      now <- getCurrentTime
      let hash = generateHistoryId 
                   dummyRunArgs
                   now
                   (CommaLogGroups (NE.fromList ["test-group"]))
                   (TimeRangeRelative (CalendarDiffTime 0 3600))
      T.length hash @?= 8
      T.all (\c -> c `elem` ("0123456789abcdef" :: String)) hash @? "Hash should contain only hex chars"
  ]