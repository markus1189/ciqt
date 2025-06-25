{-# LANGUAGE OverloadedStrings #-}

module Ciqt.History
  ( -- * History Management
    recordHistoryEntry,
    updateHistoryEntry,
    loadHistoryEntry,
    listAllHistoryEntries,
    findHistoryEntry,
    clearHistory,
    
    -- * Utilities
    generateHistoryId,
    getHistoryDirPath,
  )
where

import Ciqt.Types (ExecutionStatus (..), HistoryEntry (..), LogGroupsArg, Limit, RunArgs, TimeRange)
import Ciqt.Utils (expandTilde)
import Control.Exception (try, SomeException)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (decode', encode)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (NominalDiffTime, UTCTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))
import System.FilePath.Glob (glob)
import System.IO (stderr)

-- | Generate 8-character SHA256 hash for history entry ID
generateHistoryId :: RunArgs -> UTCTime -> LogGroupsArg -> TimeRange -> Text
generateHistoryId runArgs timestamp logGroups timeRange = 
  Text.take 8 $ Text.decodeUtf8 $ Base16.encode $ 
  SHA256.hash $ Text.encodeUtf8 $ Text.concat 
    [ Text.pack $ show runArgs
    , Text.pack $ show timestamp  
    , Text.pack $ show logGroups
    , Text.pack $ show timeRange
    ]

-- | Get the history directory path with tilde expansion
getHistoryDirPath :: Maybe FilePath -> IO FilePath
getHistoryDirPath maybeHistoryDir = do
  let defaultDir = "~/.ciqt/history"
  historyDir <- maybe defaultDir id <$> pure maybeHistoryDir
  expandTilde historyDir

-- | Record a new history entry to file
recordHistoryEntry :: 
  Maybe FilePath         -- ^ Optional history directory
  -> Text                -- ^ History ID
  -> UTCTime             -- ^ Timestamp
  -> Text                -- ^ Query text
  -> LogGroupsArg        -- ^ Log groups
  -> TimeRange           -- ^ Time range
  -> Maybe Limit         -- ^ Limit
  -> Maybe FilePath      -- ^ Query library path
  -> ExecutionStatus     -- ^ Initial status
  -> IO ()
recordHistoryEntry maybeHistoryDir historyId timestamp query logGroups timeRange limit queryLibrary status = do
  historyDir <- getHistoryDirPath maybeHistoryDir
  createDirectoryIfMissing True historyDir
  
  let entry = HistoryEntry historyId timestamp query logGroups timeRange limit queryLibrary Nothing status
      filePath = historyDir </> Text.unpack historyId <> ".json"
  
  LBS.writeFile filePath (encode entry)

-- | Update an existing history entry with execution results
updateHistoryEntry :: 
  Maybe FilePath         -- ^ Optional history directory
  -> Text                -- ^ History ID
  -> Maybe NominalDiffTime -- ^ Execution time
  -> ExecutionStatus     -- ^ Final status
  -> IO ()
updateHistoryEntry maybeHistoryDir historyId executionTime status = do
  historyDir <- getHistoryDirPath maybeHistoryDir
  let filePath = historyDir </> Text.unpack historyId <> ".json"
  
  result <- try @SomeException $ do
    content <- LBS.readFile filePath
    case decode' content of
      Nothing -> fail "Could not parse history entry"
      Just entry -> do
        let updatedEntry = entry 
              { _historyExecutionTime = executionTime
              , _historyStatus = status
              }
        LBS.writeFile filePath (encode updatedEntry)
  
  case result of
    Left err -> TIO.hPutStrLn stderr $ "Failed to update history entry: " <> Text.pack (show err)
    Right () -> pure ()

-- | Load a specific history entry by ID
loadHistoryEntry :: Maybe FilePath -> Text -> IO (Maybe HistoryEntry)
loadHistoryEntry maybeHistoryDir historyId = do
  historyDir <- getHistoryDirPath maybeHistoryDir
  let filePath = historyDir </> Text.unpack historyId <> ".json"
  
  result <- try @SomeException $ do
    content <- LBS.readFile filePath
    case decode' content of
      Nothing -> fail "Could not parse history entry"
      Just entry -> pure entry
  
  case result of
    Left _ -> pure Nothing
    Right entry -> pure (Just entry)

-- | List all history entries sorted by timestamp (newest first)
listAllHistoryEntries :: Maybe FilePath -> IO [HistoryEntry]
listAllHistoryEntries maybeHistoryDir = do
  historyDir <- getHistoryDirPath maybeHistoryDir
  exists <- doesDirectoryExist historyDir
  
  if not exists
    then pure []
    else do
      files <- listDirectory historyDir
      let jsonFiles = filter (\f -> takeExtension f == ".json") files
      
      entries <- mapM (\file -> do
        let filePath = historyDir </> file
        content <- LBS.readFile filePath
        case decode' content of
          Nothing -> pure Nothing
          Just entry -> pure (Just entry)
        ) jsonFiles
      
      -- Sort by timestamp (newest first) and return only successfully parsed entries
      let validEntries = [entry | Just entry <- entries]
      pure $ reverse $ sort $ validEntries

-- | Find history entry by partial hash (must be unique prefix)
findHistoryEntry :: Maybe FilePath -> Text -> IO (Maybe HistoryEntry)
findHistoryEntry maybeHistoryDir partialHash = do
  historyDir <- getHistoryDirPath maybeHistoryDir
  exists <- doesDirectoryExist historyDir
  
  if not exists
    then pure Nothing
    else do
      let pattern = historyDir </> Text.unpack partialHash <> "*.json"
      matches <- glob pattern
      
      case matches of
        [] -> pure Nothing
        [singleMatch] -> do
          content <- LBS.readFile singleMatch
          case decode' content of
            Nothing -> pure Nothing
            Just entry -> pure (Just entry)
        _ -> do
          TIO.hPutStrLn stderr $ "Ambiguous history ID: " <> partialHash <> " matches multiple entries"
          pure Nothing

-- | Clear all history entries
clearHistory :: Maybe FilePath -> IO ()
clearHistory maybeHistoryDir = do
  historyDir <- getHistoryDirPath maybeHistoryDir
  exists <- doesDirectoryExist historyDir
  
  if not exists
    then TIO.putStrLn "No history directory found"
    else do
      files <- listDirectory historyDir
      let jsonFiles = filter (\f -> takeExtension f == ".json") files
      
      mapM_ (\file -> removeFile (historyDir </> file)) jsonFiles
      TIO.putStrLn $ "Cleared " <> Text.pack (show (length jsonFiles)) <> " history entries"