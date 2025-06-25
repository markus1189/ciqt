{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wall #-}

module Ciqt (main) where

import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs qualified as Logs
import Amazonka.CloudWatchLogs.GetQueryResults (getQueryResultsResponse_results, getQueryResultsResponse_statistics)
import Amazonka.CloudWatchLogs.Lens (putQueryDefinitionResponse_queryDefinitionId)
import Ciqt.AWS (calculateLogGroups, discoverAwsEnv, listAWSQueries, deleteAWSQuery, syncQueries, getAWSQueryById)
import Ciqt.CLI (parseAppArgs)
import Ciqt.History (recordHistoryEntry, updateHistoryEntry, listAllHistoryEntries, findHistoryEntry, clearHistory, generateHistoryId)
import Ciqt.Library (calculateQueryFromArg, listQueries, saveQuery, deleteQuery)
import Ciqt.Query (buildQuery, executeQuery, calculateQueryStartEnd, calculateLimitFromRunArgs, calculateQueryFromRunArgs)
import Ciqt.Types
import Ciqt.Utils (parseNestedJson, resultFieldsToJson, printPreFlightInfo, formatQueryStats)
import Control.Exception (SomeException, try)
import Control.Lens ((&), (^.), (%~))
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (diffUTCTime, getCurrentTime, getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stderr, stdout)
import System.Log.FastLogger (FastLogger, LogType' (LogStderr), defaultBufSize, withFastLogger)

-- | Main entry point with exception handling
main :: IO ()
main = do
  result <- try @SomeException $ mainProgram
  case result of
    Left _ -> exitFailure
    Right () -> exitSuccess

-- | Main program logic
mainProgram :: IO ()
mainProgram = withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
  appArgs <- parseAppArgs

  case appArgs ^. appArgsCommand of
    RunCommand runArgs -> handleRunCommand fastLogger runArgs (appArgs ^. appArgsGlobalQueryLibrary) (appArgs ^. appArgsGlobalHistoryDir)
    LibraryCommand libArgs -> handleLibraryCommand libArgs (appArgs ^. appArgsGlobalQueryLibrary)
    QueryShowCommand queryArgs -> handleQueryShowCommand queryArgs (appArgs ^. appArgsGlobalQueryLibrary)
    HistoryCommand historyArgs -> handleHistoryCommand historyArgs (appArgs ^. appArgsGlobalHistoryDir)

-- | Handle run command execution
handleRunCommand :: FastLogger -> RunArgs -> Maybe FilePath -> Maybe FilePath -> IO ()
handleRunCommand fastLogger runArgs globalLibPath globalHistoryDir = do
  let queryLibPath = runArgs ^. runArgsQueryLibrary <|> globalLibPath

  -- For actual query execution, validate required parameters
  logGroupsArg <- case runArgs ^. runArgsLogGroups of
    Nothing -> do
      TIO.hPutStrLn stderr "Error: Log groups specification is required for query execution"
      exitFailure
    Just lgs -> pure lgs

  timeRange <- case runArgs ^. runArgsTimeRange of
    Nothing -> do
      TIO.hPutStrLn stderr "Error: Time range specification is required for query execution"
      exitFailure
    Just tr -> pure tr

  tz <- getCurrentTimeZone
  now <- liftIO getCurrentTime
  env <- discoverAwsEnv fastLogger

  maybeLgs <- calculateLogGroups env logGroupsArg
  lgs <- case maybeLgs of
    Nothing -> do
      TIO.hPutStrLn stderr $ "Could not find any log groups from " <> showText logGroupsArg
      exitFailure
    Just lgs ->
      pure lgs

  let (queryStart, queryEnd) = calculateQueryStartEnd now timeRange
      limit = calculateLimitFromRunArgs runArgs

  query <- calculateQueryFromRunArgs runArgs queryLibPath

  let awsQuery = buildQuery limit lgs queryStart queryEnd query

  -- Generate history entry
  let entryHistoryId = generateHistoryId runArgs now logGroupsArg timeRange
      status = if runArgs ^. runArgsDryRun then DryRun else Success
      limitForHistory = runArgs ^. runArgsLimit  -- Use original Limit type

  -- Record initial history entry
  recordHistoryEntry globalHistoryDir entryHistoryId now query logGroupsArg timeRange limitForHistory queryLibPath status

  printPreFlightInfo env tz (queryStart, queryEnd) lgs limit query

  unless (runArgs ^. runArgsDryRun) $ do
    startTime <- getCurrentTime
    result <- try @SomeException $ AWS.runResourceT $ do
      liftIO $ TIO.hPutStrLn stderr "---------------------------"
      mresult <- executeQuery env awsQuery
      for_ mresult $ \result -> do
        let mlogEvents = result ^. getQueryResultsResponse_results
            stats = result ^. getQueryResultsResponse_statistics
        for_ mlogEvents $ \logEvents -> for_ logEvents $ \logEvent -> do
          let logEvent' = resultFieldsToJson logEvent & key "@message" %~ parseNestedJson
          liftIO . LBS.hPutStrLn stdout . Aeson.encode $ logEvent'

        liftIO $ do
          hFlush stdout
          TIO.hPutStrLn stderr $ "---------------------------\nStatistics:\n" <> maybe "" formatQueryStats stats <> "\n---------------------------"
    
    endTime <- getCurrentTime
    let executionTime = diffUTCTime endTime startTime
        finalStatus = case result of
          Left _ -> Failed
          Right _ -> Success
    
    -- Update history entry with execution results
    updateHistoryEntry globalHistoryDir entryHistoryId (Just executionTime) finalStatus

-- | Handle library command operations
handleLibraryCommand :: LibraryArgs -> Maybe FilePath -> IO ()
handleLibraryCommand libArgs globalLibPath = do
  let queryLibPath = libArgs ^. libraryArgsQueryLibrary <|> globalLibPath

  case libArgs ^. libraryArgsOperation of
    ListQueries -> do
      listQueries queryLibPath
      exitSuccess
    SaveQuery name queryArg -> do
      query <- calculateQueryFromArg queryArg queryLibPath
      saveQuery queryLibPath name query
      exitSuccess
    DeleteQuery name -> do
      deleteQuery queryLibPath name
      exitSuccess
    ShowQuery name -> do
      query <- calculateQueryFromArg (QueryLibrary name) queryLibPath
      TIO.putStrLn query
      exitSuccess
    ListAWSQueries -> do
      withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
        env <- discoverAwsEnv fastLogger
        listAWSQueries env
      exitSuccess
    DownloadAWSQuery queryId localName -> do
      withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
        env <- discoverAwsEnv fastLogger
        downloadAWSQueryImpl env queryLibPath queryId localName
      exitSuccess
    UploadQuery localName -> do
      withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
        env <- discoverAwsEnv fastLogger
        uploadLocalQueryImpl env queryLibPath localName
      exitSuccess
    DeleteAWSQuery queryId -> do
      withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
        env <- discoverAwsEnv fastLogger
        deleteAWSQuery env queryId
      exitSuccess
    SyncQueries -> do
      withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
        env <- discoverAwsEnv fastLogger
        syncQueries env queryLibPath
      exitSuccess

-- | Handle query show command
handleQueryShowCommand :: QueryShowArgs -> Maybe FilePath -> IO ()
handleQueryShowCommand queryArgs globalLibPath = do
  let queryLibPath = queryArgs ^. queryShowArgsQueryLibrary <|> globalLibPath
  query <- calculateQueryFromArg (queryArgs ^. queryShowArgsQuery) queryLibPath
  TIO.putStrLn query
  exitSuccess

-- | Handle history command operations
handleHistoryCommand :: HistoryArgs -> Maybe FilePath -> IO ()
handleHistoryCommand historyArgs globalHistoryDir = do
  let historyDir = historyArgs ^. historyArgsHistoryDir <|> globalHistoryDir

  case historyArgs ^. historyArgsOperation of
    ListHistory -> do
      entries <- listAllHistoryEntries historyDir
      if null entries
        then TIO.putStrLn "No history entries found"
        else do
          TIO.putStrLn "History entries (newest first):"
          mapM_ printHistoryEntrySummary entries
      exitSuccess
    
    ShowHistory hash -> do
      maybeEntry <- findHistoryEntry historyDir hash
      case maybeEntry of
        Nothing -> do
          TIO.hPutStrLn stderr $ "History entry not found: " <> hash
          exitFailure
        Just entry -> do
          printHistoryEntryDetails entry
          exitSuccess
    
    RerunHistory hash -> do
      maybeEntry <- findHistoryEntry historyDir hash
      case maybeEntry of
        Nothing -> do
          TIO.hPutStrLn stderr $ "History entry not found: " <> hash
          exitFailure
        Just entry -> do
          TIO.putStrLn $ "Re-executing query from history: " <> _historyId entry
          -- Create new RunArgs from history entry and re-execute
          let runArgs = RunArgs
                { _runArgsQuery = QueryString (_historyQuery entry)
                , _runArgsLimit = _historyLimit entry
                , _runArgsTimeRange = Just (_historyTimeRange entry)
                , _runArgsLogGroups = Just (_historyLogGroups entry)
                , _runArgsDryRun = False
                , _runArgsQueryLibrary = _historyQueryLibrary entry
                }
          -- Use a dummy FastLogger for rerun
          withFastLogger (LogStderr defaultBufSize) $ \fastLogger ->
            handleRunCommand fastLogger runArgs (_historyQueryLibrary entry) historyDir
    
    ClearHistory -> do
      clearHistory historyDir
      exitSuccess

-- | Print a summary line for a history entry
printHistoryEntrySummary :: HistoryEntry -> IO ()
printHistoryEntrySummary entry = do
  let hashStr = _historyId entry
      timestamp = Text.pack $ show $ _historyTimestamp entry
      queryPreview = Text.take 50 $ Text.replace "\n" " " $ _historyQuery entry
      statusStr = Text.pack $ show $ _historyStatus entry
      executionTimeStr = case _historyExecutionTime entry of
        Nothing -> "N/A"
        Just time -> Text.pack $ show (round time :: Int) <> "s"
  
  TIO.putStrLn $ Text.intercalate " | " 
    [ hashStr
    , timestamp
    , statusStr
    , executionTimeStr
    , queryPreview <> "..."
    ]

-- | Print detailed information for a history entry
printHistoryEntryDetails :: HistoryEntry -> IO ()
printHistoryEntryDetails entry = do
  TIO.putStrLn $ "History ID: " <> _historyId entry
  TIO.putStrLn $ "Timestamp: " <> Text.pack (show $ _historyTimestamp entry)
  TIO.putStrLn $ "Status: " <> Text.pack (show $ _historyStatus entry)
  TIO.putStrLn $ "Execution Time: " <> case _historyExecutionTime entry of
    Nothing -> "N/A"
    Just time -> Text.pack (show time) <> " seconds"
  TIO.putStrLn $ "Log Groups: " <> Text.pack (show $ _historyLogGroups entry)
  TIO.putStrLn $ "Time Range: " <> Text.pack (show $ _historyTimeRange entry)
  TIO.putStrLn $ "Limit: " <> case _historyLimit entry of
    Nothing -> "None"
    Just limit -> Text.pack (show limit)
  TIO.putStrLn $ "Query Library: " <> case _historyQueryLibrary entry of
    Nothing -> "Default"
    Just path -> Text.pack path
  TIO.putStrLn "Query:"
  TIO.putStrLn $ _historyQuery entry

-- | Helper function to show types as Text (temporary)
showText :: Show a => a -> Text
showText = Text.pack . show

-- | Download AWS query to local library (implementation to avoid circular deps)
downloadAWSQueryImpl :: AWS.Env -> Maybe FilePath -> Text -> Text -> IO ()
downloadAWSQueryImpl env queryLibPath queryId localName = do
  queryStr <- getAWSQueryById env queryId
  saveQuery queryLibPath localName queryStr
  TIO.putStrLn $ "Downloaded AWS query " <> queryId <> " as: " <> localName

-- | Upload local query to AWS (implementation to avoid circular deps)
uploadLocalQueryImpl :: AWS.Env -> Maybe FilePath -> Text -> IO ()
uploadLocalQueryImpl env queryLibPath localName = do
  queryStr <- calculateQueryFromArg (QueryLibrary localName) queryLibPath
  AWS.runResourceT $ do
    let putQuery = Logs.newPutQueryDefinition localName queryStr
    res <- AWS.send env putQuery
    case res ^. putQueryDefinitionResponse_queryDefinitionId of
      Nothing -> liftIO $ TIO.putStrLn $ "Failed to upload query: " <> localName
      Just queryId -> liftIO $ TIO.putStrLn $ "Uploaded query " <> localName <> " with ID: " <> queryId