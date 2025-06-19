{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wall #-}

import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs (QueryStatus (..))
import Amazonka.CloudWatchLogs qualified as Logs
import Amazonka.CloudWatchLogs.GetQueryResults (getQueryResultsResponse_results, getQueryResultsResponse_statistics)
import Amazonka.CloudWatchLogs.Lens (describeLogGroupsResponse_logGroups, describeLogGroupsResponse_nextToken, describeLogGroups_logGroupNamePattern, describeLogGroups_logGroupNamePrefix, describeLogGroups_nextToken, describeQueryDefinitionsResponse_nextToken, describeQueryDefinitionsResponse_queryDefinitions, describeQueryDefinitions_nextToken, getQueryResultsResponse_status, logGroup_logGroupName, putQueryDefinitionResponse_queryDefinitionId, queryDefinition_logGroupNames, queryDefinition_name, queryDefinition_queryDefinitionId, queryDefinition_queryString, resultField_field, resultField_value, startQueryResponse_queryId, startQuery_limit)
import Amazonka.CloudWatchLogs.StartQuery (startQuery_logGroupNames)
import Amazonka.CloudWatchLogs.StopQuery (newStopQuery)
import Amazonka.CloudWatchLogs.Types (QueryDefinition, QueryStatistics (QueryStatistics'), ResultField, bytesScanned, recordsMatched, recordsScanned)
import Amazonka.Prelude (mapMaybe)
import Control.Applicative ((<|>))
import Control.Exception.Lens (trying)
import Control.Lens (view, (%~))
import Control.Lens.Operators ((&), (.~), (<&>), (?~), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, SomeException, bracket, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Fail (FailT, runFailT)
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Control.Retry (constantDelay, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either.Combinators (mapLeft)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (CalendarDiffTime, NominalDiffTime, TimeZone, UTCTime, ZonedTime, addUTCTime, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM)
import Data.Traversable (for)
import Fmt (blockListF, build, commaizeF, dateDashF, hmsF, indentF, padLeftF, padRightF, tzF, (+|), (|+))
import Formatting (sformat, (%), (%.))
import Formatting.Combinators qualified as Formatters
import Formatting.Formatters qualified as Formatters
import NeatInterpolation (trimming)
import Numeric.Natural (Natural)
import Options.Applicative
  ( Parser,
    auto,
    command,
    eitherReader,
    execParser,
    footer,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    progDesc,
    showDefault,
    strArgument,
    strOption,
    subparser,
    switch,
    value,
    (<**>),
  )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory, removeFile)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, makeRelative, takeDirectory, takeExtension, (</>))
import System.FilePath.Glob (Pattern, compDefault, tryCompileWith)
import System.FilePath.Glob qualified as Glob
import System.IO (hFlush, stderr, stdout)
import System.Log.FastLogger (FastLogger, LogType' (LogStderr), ToLogStr (toLogStr), defaultBufSize, withFastLogger)
import Text.Regex.PCRE (Regex, RegexContext (match), RegexMaker (makeRegexM))

data LogGroupsArg
  = CommaLogGroups (NonEmpty Text)
  | LogNamePattern Text
  | LogNamePrefix Text
  | LogNameGlob Pattern
  | LogNameRegex (Text, Regex)

instance Show LogGroupsArg where
  show (CommaLogGroups lgs) = "CommaLogGroups " +| show lgs |+ ""
  show (LogNamePattern p) = "LogNamePattern " +| show p |+ ""
  show (LogNamePrefix s) = "LogNamePrefix " +| show s |+ ""
  show (LogNameGlob g) = "LogNameGlob " +| show g |+ ""
  show (LogNameRegex (r, _)) = "LogNameRegex " +| show r |+ ""

data TimeRange = TimeRangeAbsolute ZonedTime (Maybe ZonedTime) | TimeRangeRelative CalendarDiffTime deriving (Show)

data Limit = ExplicitLimit Natural | MaxLimit deriving (Eq, Show)

data QueryArg = QueryFile FilePath | QueryString Text | QueryLibrary Text | QueryAWS Text deriving (Eq, Show)

data RunArgs = RunArgs
  { _runArgsQuery :: QueryArg,
    _runArgsLimit :: Maybe Limit,
    _runArgsTimeRange :: Maybe TimeRange,
    _runArgsLogGroups :: Maybe LogGroupsArg,
    _runArgsDryRun :: Bool,
    _runArgsQueryLibrary :: Maybe FilePath
  }
  deriving (Show)

data LibraryOperation
  = ListQueries
  | SaveQuery Text QueryArg
  | DeleteQuery Text
  | ShowQuery Text
  | ListAWSQueries
  | DownloadAWSQuery Text Text -- AWS query ID, local name
  | UploadQuery Text -- local name to upload to AWS
  | DeleteAWSQuery Text -- AWS query ID
  | SyncQueries -- sync between local and AWS
  deriving (Show)

data LibraryArgs = LibraryArgs
  { _libraryArgsOperation :: LibraryOperation,
    _libraryArgsQueryLibrary :: Maybe FilePath
  }
  deriving (Show)

data QueryShowArgs = QueryShowArgs
  { _queryShowArgsQuery :: QueryArg,
    _queryShowArgsQueryLibrary :: Maybe FilePath
  }
  deriving (Show)

data Command
  = RunCommand RunArgs
  | LibraryCommand LibraryArgs
  | QueryShowCommand QueryShowArgs
  deriving (Show)

data AppArgs = AppArgs
  { _appArgsCommand :: Command,
    _appArgsGlobalQueryLibrary :: Maybe FilePath
  }
  deriving (Show)

makeLenses ''RunArgs
makeLenses ''LibraryArgs
makeLenses ''QueryShowArgs
makeLenses ''AppArgs

appArgsParser :: Parser AppArgs
appArgsParser =
  AppArgs
    <$> commandParser
    <*> optional
      ( strOption
          ( long "query-library"
              <> metavar "DIR"
              <> help "Global path to directory containing saved queries (defaults to ~/.ciqt/queries)"
          )
      )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "run"
        ( info
            (RunCommand <$> runArgsParser <**> helper)
            (progDesc "Execute CloudWatch Insights queries against specified log groups and time ranges")
        )
        <> command
          "library"
          ( info
              (LibraryCommand <$> libraryArgsParser <**> helper)
              (progDesc "Manage saved queries in your query library (~/.ciqt/queries by default)")
          )
        <> command
          "query"
          ( info
              (QueryShowCommand <$> queryArgsParser <**> helper)
              (progDesc "Display query content without execution (useful for validation)")
          )
    )
    <|> RunCommand <$> runArgsParser -- Default to run command for backward compatibility

runArgsParser :: Parser RunArgs
runArgsParser =
  RunArgs
    <$> queryArgParser
    <*> limitParser
    <*> optional timeRangeParser
    <*> optional logGroupsParser
    <*> switch (long "dry-run" <> help "Print query details and arguments without executing the query")
    <*> optional
      ( strOption
          ( long "query-library"
              <> metavar "DIR"
              <> help "Path to directory containing saved queries (defaults to ~/.ciqt/queries)"
          )
      )

libraryArgsParser :: Parser LibraryArgs
libraryArgsParser =
  LibraryArgs
    <$> libraryOperationParser
    <*> optional
      ( strOption
          ( long "query-library"
              <> metavar "DIR"
              <> help "Path to directory containing saved queries (defaults to ~/.ciqt/queries)"
          )
      )

libraryOperationParser :: Parser LibraryOperation
libraryOperationParser =
  subparser
    ( command
        "list"
        ( info
            (pure ListQueries <**> helper)
            (progDesc "List all available queries in the library with directory structure")
        )
        <> command
          "save"
          ( info
              (saveQueryParser <**> helper)
              (progDesc "Save a query to the library (supports nested directories like 'aws/lambda/errors')")
          )
        <> command
          "delete"
          ( info
              (deleteQueryParser <**> helper)
              (progDesc "Delete a saved query from the library")
          )
        <> command
          "show"
          ( info
              (showQueryParser <**> helper)
              (progDesc "Display the content of a saved query without executing it")
          )
        <> command
          "aws-list"
          ( info
              (pure ListAWSQueries <**> helper)
              (progDesc "List all saved queries from AWS CloudWatch Logs Insights")
          )
        <> command
          "aws-download"
          ( info
              (downloadAWSQueryParser <**> helper)
              (progDesc "Download an AWS saved query to local library")
          )
        <> command
          "aws-upload"
          ( info
              (uploadQueryParser <**> helper)
              (progDesc "Upload a local query to AWS CloudWatch Logs Insights")
          )
        <> command
          "aws-delete"
          ( info
              (deleteAWSQueryParser <**> helper)
              (progDesc "Delete a saved query from AWS CloudWatch Logs Insights")
          )
        <> command
          "sync"
          ( info
              (pure SyncQueries <**> helper)
              (progDesc "Sync queries between local library and AWS")
          )
    )

saveQueryParser :: Parser LibraryOperation
saveQueryParser =
  SaveQuery
    <$> strArgument (metavar "NAME" <> help "Name/path to save the query as (e.g., 'aws/lambda/errors')")
    <*> queryArgParser

deleteQueryParser :: Parser LibraryOperation
deleteQueryParser =
  DeleteQuery
    <$> strArgument (metavar "NAME" <> help "Name/path of query to delete (e.g., 'aws/lambda/errors')")

showQueryParser :: Parser LibraryOperation
showQueryParser =
  ShowQuery
    <$> strArgument (metavar "NAME" <> help "Name/path of query to display (e.g., 'aws/lambda/errors')")

downloadAWSQueryParser :: Parser LibraryOperation
downloadAWSQueryParser =
  DownloadAWSQuery
    <$> strArgument (metavar "QUERY_ID" <> help "AWS query definition ID to download")
    <*> strArgument (metavar "LOCAL_NAME" <> help "Local name to save the query as (e.g., 'aws/lambda/errors')")

uploadQueryParser :: Parser LibraryOperation
uploadQueryParser =
  UploadQuery
    <$> strArgument (metavar "LOCAL_NAME" <> help "Local query name to upload to AWS (e.g., 'aws/lambda/errors')")

deleteAWSQueryParser :: Parser LibraryOperation
deleteAWSQueryParser =
  DeleteAWSQuery
    <$> strArgument (metavar "QUERY_ID" <> help "AWS query definition ID to delete")

queryArgsParser :: Parser QueryShowArgs
queryArgsParser =
  QueryShowArgs
    <$> queryArgParser
    <*> optional
      ( strOption
          ( long "query-library"
              <> metavar "DIR"
              <> help "Path to directory containing saved queries (defaults to ~/.ciqt/queries)"
          )
      )

queryArgParser :: Parser QueryArg
queryArgParser = queryFileParser <|> queryStringParser <|> queryLibraryParser <|> queryAWSParser
  where
    queryFileParser =
      QueryFile
        <$> strOption
          ( long "query-file"
              <> metavar "FILE"
              <> help "Path to a file containing a CloudWatch Insights query"
          )

    queryStringParser =
      QueryString
        <$> strOption
          ( long "query"
              <> metavar "QUERY"
              <> help "CloudWatch Insights query string to execute"
              <> showDefault
              <> value "fields @timestamp, @message, @logStream, @log | sort @timestamp desc"
          )

    queryLibraryParser =
      QueryLibrary
        <$> strOption
          ( long "query-name"
              <> metavar "NAME"
              <> help "Name/path of saved query from library (e.g., 'aws/lambda/errors')"
          )

    queryAWSParser =
      QueryAWS
        <$> strOption
          ( long "query-aws-id"
              <> metavar "QUERY_ID"
              <> help "AWS CloudWatch Logs Insights saved query definition ID"
          )

timeRangeParser :: Parser TimeRange
timeRangeParser = timeRangeAbsolute <|> timeRangeRelative
  where
    timeRangeAbsolute =
      TimeRangeAbsolute
        <$> option
          (maybeReader (formatParseM iso8601Format))
          (long "start" <> metavar "TIME" <> help "Query start time in ISO8601 format (e.g., 2023-01-01T00:00:00Z)")
        <*> optional
          ( option
              (maybeReader (formatParseM iso8601Format))
              (long "end" <> metavar "TIME" <> help "Query end time in ISO8601 format (defaults to current time)")
          )

    timeRangeRelative =
      TimeRangeRelative
        <$> option
          (maybeReader (formatParseM iso8601Format))
          (long "since" <> metavar "ISO8601_DURATION" <> help "Query time range as ISO8601 duration (e.g., P1D for 1 day, PT1H for 1 hour)")

limitParser :: Parser (Maybe Limit)
limitParser =
  optional
    ( ExplicitLimit
        <$> option auto (long "limit" <> metavar "POSINT" <> help "Maximum number of log entries to return")
        <|> MaxLimit <$ switch (long "limit-max" <> help "Use maximum limit (10,000) for results from AWS")
    )

logGroupsParser :: Parser LogGroupsArg
logGroupsParser =
  CommaLogGroups
    <$> option
      (maybeReader (NonEmpty.nonEmpty . map Text.pack . splitOn ","))
      ( long "log-groups" <> metavar "LOG_GROUPS" <> help "Comma-separated list of CloudWatch log groups (e.g., '/aws/lambda/func1,/aws/lambda/func2')"
      )
    <|> LogNamePattern
      <$> strOption
        ( long "log-group-pattern" <> metavar "PATTERN" <> help "Match log groups containing this substring (case-sensitive)"
        )
    <|> LogNamePrefix
      <$> strOption
        ( long "log-group-prefix" <> metavar "PREFIX" <> help "Match log groups starting with this prefix (case-sensitive)"
        )
    <|> LogNameGlob
      <$> option
        (eitherReader (\t -> mapLeft (\err -> "Could not compile glob: " +| t |+ ". Error was: " +| err |+ "") . tryCompileWith compDefault $ t))
        ( long "log-group-glob" <> metavar "GLOB" <> help "Match log groups using glob pattern (e.g., '/aws/lambda/*' - WARNING: fetches ALL log groups first)"
        )
    <|> LogNameRegex
      <$> option
        (eitherReader (\t -> fmap (Text.pack t,) . mapLeft (\err -> "Could not parse regex: " +| t |+ ". Error was: " +| err |+ "") . runIdentity . runFailT . makeRegexM @Regex @_ @_ @_ @(FailT String Identity) $ t))
        ( long "log-group-regex" <> metavar "REGEX" <> help "Match log groups using PCRE regex (e.g., '/aws/lambda/.*' - WARNING: fetches ALL log groups first)"
        )

fastLoggerToAwsLogger :: FastLogger -> AWS.Logger
fastLoggerToAwsLogger fastLogger lvl bss = when (lvl <= AWS.Error) $ fastLogger (toLogStr bss)

main :: IO ()
main = do
  result <- try @_ @SomeException $ mainProgram
  case result of
    Left _ -> exitFailure
    Right () -> exitSuccess

discoverAwsEnv :: (MonadIO m, MonadCatch m) => FastLogger -> m AWS.Env
discoverAwsEnv logger = do
  env <- AWS.newEnv AWS.discover
  pure $ env {AWS.logger = fastLoggerToAwsLogger logger}

mainProgram :: IO ()
mainProgram = withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
  let opts =
        info
          (appArgsParser <**> helper)
          ( fullDesc
              <> progDesc "Execute and manage CloudWatch Insights queries with flexible log group selection and time ranges"
              <> header "ciqt - CloudWatch Insights Query Tool"
              <> footer "Examples:\n  ciqt run --query 'fields @timestamp, @message | limit 10' --log-groups '/aws/lambda/my-function' --start 2023-01-01T00:00:00Z\n  ciqt library list\n  ciqt library save my-query --query 'fields @timestamp | limit 100'"
          )
  appArgs <- execParser opts

  case appArgs ^. appArgsCommand of
    RunCommand runArgs -> handleRunCommand fastLogger runArgs (appArgs ^. appArgsGlobalQueryLibrary)
    LibraryCommand libArgs -> handleLibraryCommand libArgs (appArgs ^. appArgsGlobalQueryLibrary)
    QueryShowCommand queryArgs -> handleQueryShowCommand queryArgs (appArgs ^. appArgsGlobalQueryLibrary)

handleRunCommand :: FastLogger -> RunArgs -> Maybe FilePath -> IO ()
handleRunCommand fastLogger runArgs globalLibPath = do
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
      fastLogger . toLogStr @Text $ "Could not find any log groups from " +| show logGroupsArg |+ ""
      exitFailure
    Just lgs ->
      pure lgs

  let (queryStart, queryEnd) = calculateQueryStartEnd now timeRange
      limit = calculateLimitFromRunArgs runArgs

  query <- calculateQueryFromRunArgs runArgs queryLibPath

  let awsQuery = buildQuery limit lgs queryStart queryEnd query

  printPreFlightInfo env tz (queryStart, queryEnd) lgs limit query

  unless (runArgs ^. runArgsDryRun) . AWS.runResourceT $ do
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
        TIO.hPutStrLn stderr $ "---------------------------\nStatistics:\n" +| fmap formatQueryStats stats |+ "\n---------------------------"

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
        downloadAWSQuery env queryLibPath queryId localName
      exitSuccess
    UploadQuery localName -> do
      withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
        env <- discoverAwsEnv fastLogger
        uploadLocalQuery env queryLibPath localName
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

handleQueryShowCommand :: QueryShowArgs -> Maybe FilePath -> IO ()
handleQueryShowCommand queryArgs globalLibPath = do
  let queryLibPath = queryArgs ^. queryShowArgsQueryLibrary <|> globalLibPath
  query <- calculateQueryFromArg (queryArgs ^. queryShowArgsQuery) queryLibPath
  TIO.putStrLn query
  exitSuccess

calculateLogGroups :: AWS.Env -> LogGroupsArg -> IO (Maybe (NonEmpty Text))
calculateLogGroups _ (CommaLogGroups lgs) = pure (Just lgs)
calculateLogGroups env (LogNamePattern p) = AWS.runResourceT $ do
  let query = Logs.newDescribeLogGroups & describeLogGroups_logGroupNamePattern ?~ p
  res <- AWS.send env query
  pure . (=<<) NonEmpty.nonEmpty . fmap (mapMaybe (view logGroup_logGroupName)) $ res ^. describeLogGroupsResponse_logGroups
calculateLogGroups env (LogNamePrefix s) = AWS.runResourceT $ do
  let query = Logs.newDescribeLogGroups & describeLogGroups_logGroupNamePrefix ?~ s
  res <- AWS.send env query
  pure . (=<<) NonEmpty.nonEmpty . fmap (mapMaybe (view logGroup_logGroupName)) $ res ^. describeLogGroupsResponse_logGroups
calculateLogGroups env (LogNameGlob patternString) = AWS.runResourceT $ do
  lgs <- describeAllLogGroups env Nothing
  pure $ NonEmpty.nonEmpty . mapMaybe (view logGroup_logGroupName) $ filter (globLogGroupName patternString) lgs
calculateLogGroups env (LogNameRegex (_, regex)) = AWS.runResourceT $ do
  lgs <- describeAllLogGroups env Nothing
  let filtered = filter (\lg -> maybe False (match regex . Text.unpack) (lg ^. logGroup_logGroupName)) lgs
  pure $ NonEmpty.nonEmpty . mapMaybe (view logGroup_logGroupName) $ filtered

calculateQueryStartEnd :: UTCTime -> TimeRange -> (UTCTime, UTCTime)
calculateQueryStartEnd now timeRange =
  case timeRange of
    TimeRangeAbsolute s e -> (zonedTimeToUTC s, maybe now zonedTimeToUTC e)
    TimeRangeRelative d -> (addUTCTime (negate . fromInteger @NominalDiffTime . read $ formatTime defaultTimeLocale "%s" d) now, now)

calculateLimitFromRunArgs :: RunArgs -> Maybe Natural
calculateLimitFromRunArgs runArgs =
  runArgs ^. runArgsLimit <&> \case
    MaxLimit -> 10000
    ExplicitLimit x -> x

calculateQueryFromRunArgs :: RunArgs -> Maybe FilePath -> IO Text
calculateQueryFromRunArgs runArgs queryLibPath =
  calculateQueryFromArg (runArgs ^. runArgsQuery) queryLibPath

calculateQueryFromArg :: QueryArg -> Maybe FilePath -> IO Text
calculateQueryFromArg queryArg queryLibPath = case queryArg of
  QueryString q -> pure q
  QueryFile f -> TIO.readFile f
  QueryLibrary name -> do
    let defaultDir = "~/.ciqt/queries"
    libPath <- maybe defaultDir id <$> pure queryLibPath
    expandedPath <- expandTilde libPath
    let queryPath = expandedPath </> Text.unpack name <> ".query"
    exists <- doesFileExist queryPath
    if exists
      then TIO.readFile queryPath
      else fail $ "Query '" ++ Text.unpack name ++ "' not found in library: " ++ expandedPath
  QueryAWS queryId -> do
    withFastLogger (LogStderr defaultBufSize) $ \fastLogger -> do
      env <- discoverAwsEnv fastLogger
      getAWSQueryById env queryId

-- | Expand ~ in file paths to the user's home directory
expandTilde :: FilePath -> IO FilePath
expandTilde path = case path of
  '~' : '/' : rest -> do
    home <- getHomeDirectory
    return $ home </> rest
  _ -> return path

buildQuery :: Maybe Natural -> NonEmpty Text -> UTCTime -> UTCTime -> Text -> Logs.StartQuery
buildQuery n lgs qstart qend q =
  Logs.newStartQuery (toSeconds qstart) (toSeconds qend) q
    & startQuery_logGroupNames ?~ NonEmpty.toList lgs
    & startQuery_limit .~ n
  where
    toSeconds = round . utcTimeToPOSIXSeconds

parseNestedJson :: Value -> Value
parseNestedJson v@(Aeson.String s) = Data.Maybe.fromMaybe v (Aeson.decode @Value $ toLazyByteString $ Text.encodeUtf8Builder s)
parseNestedJson v = v

resultFieldsToJson :: [ResultField] -> Aeson.Value
resultFieldsToJson rfs = Aeson.toJSON $ Map.fromList $ mapMaybe resultFieldToJson rfs

resultFieldToJson :: ResultField -> Maybe (Text, Text)
resultFieldToJson rf = (,) <$> view resultField_field rf <*> view resultField_value rf

executeQuery :: AWS.Env -> Logs.StartQuery -> ResourceT IO (Maybe Logs.GetQueryResultsResponse)
executeQuery env query = bracket acquire release act
  where
    acquire :: ResourceT IO Logs.StartQueryResponse
    acquire = AWS.send env query

    release :: Logs.StartQueryResponse -> ResourceT IO ()
    release startQueryResponse = do
      let qid = startQueryResponse ^. startQueryResponse_queryId
      case qid of
        Nothing -> pure ()
        Just queryId -> do
          queryStatus <- view getQueryResultsResponse_status <$> AWS.send env (Logs.newGetQueryResults queryId)
          case queryStatus of
            Just QueryStatus_Running -> do
              liftIO $ TIO.hPutStrLn stderr [trimming|Stopping query: $queryId|]
              r <- trying AWS._ServiceError $ AWS.send env (newStopQuery queryId)
              case r of
                Left _ -> liftIO $ TIO.hPutStrLn stderr [trimming|Failed stopping query: $queryId|]
                Right _ -> liftIO $ TIO.hPutStrLn stderr [trimming|Suceeded stopping query: $queryId|]
            _ -> pure ()

    act :: (MonadResource m, MonadIO m) => Logs.StartQueryResponse -> m (Maybe Logs.GetQueryResultsResponse)
    act res = do
      let queryId = res ^. startQueryResponse_queryId

      for queryId $ \qid -> do
        liftIO . TIO.hPutStrLn stderr $ "QueryId: " <> qid
        retrying
          (limitRetriesByCumulativeDelay (30 * 60 * 1000 * 1000) (constantDelay (2 * 1000 * 1000)))
          ( \_ response -> case response ^. getQueryResultsResponse_status of
              Just QueryStatus_Running -> do
                liftIO $ TIO.hPutStrLn stderr $ "Query is RUNNING: " +| fmap formatQueryStats (response ^. getQueryResultsResponse_statistics) |+ ""
                pure True
              Just QueryStatus_Scheduled -> do
                liftIO $ TIO.hPutStrLn stderr "Query is SCHEDULED"
                pure True
              Just QueryStatus_Complete -> pure False
              x -> do
                liftIO $ TIO.hPutStrLn stderr ("Unknown Query Status: " +| fmap show x |+ "")
                pure False
          )
          (\_ -> AWS.send env $ Logs.newGetQueryResults qid)

formatQueryStats :: QueryStatistics -> Text
formatQueryStats QueryStatistics' {recordsScanned = scanned, bytesScanned = bytesS, recordsMatched = matched} =
  Text.intercalate
    " / "
    [ "Records matched: " +| padLeftF 13 ' ' (commaizeF @Integer . round <$> matched),
      "Records scanned: " +| padLeftF 13 ' ' (commaizeF @Integer . round <$> scanned),
      sformat ("Bytes Scanned: " % (Formatters.left 13 ' ' %. Formatters.maybed "" (Formatters.bytes @Double @Integer (Formatters.fixed 2)))) (fmap round bytesS)
    ]

printPreFlightInfo :: AWS.Env -> TimeZone -> (UTCTime, UTCTime) -> NonEmpty Text -> Maybe Natural -> Text -> IO ()
printPreFlightInfo awsEnv tz (queryStart, queryEnd) logGroups limit query =
  TIO.hPutStrLn
    stderr
    ( Text.intercalate
        "\n"
        [ padRightF @Text 14 ' ' "AWS Region: " +| AWS.fromRegion (AWS.region awsEnv) |+ "",
          padRightF @Text 14 ' ' "Query Start: " +| dateDashF (utcToZonedTime tz queryStart) |+ "T" +| hmsF (utcToZonedTime tz queryStart) |+ tzF (utcToZonedTime tz queryStart),
          padRightF @Text 14 ' ' "Query End: " +| dateDashF (utcToZonedTime tz queryEnd) |+ "T" +| hmsF (utcToZonedTime tz queryEnd) |+ tzF (utcToZonedTime tz queryEnd),
          padRightF @Text 14 ' ' "Interval: " +| Text.pack (formatTime defaultTimeLocale "%d days, %H hours, %M minutes, %S seconds" $ diffUTCTime queryEnd queryStart) |+ "",
          padRightF @Text 14 ' ' "Limit: " +| maybe "<no-limit-specified>" (commaizeF . fromIntegral @Natural @Integer) limit,
          "LogGroups:\n\n" +| indentF 2 (blockListF logGroups),
          "Query:\n\n" +| indentF 2 (build query),
          "---------------------------"
        ]
    )

describeAllLogGroups :: (MonadResource m) => AWS.Env -> Maybe Text -> m [Logs.LogGroup]
describeAllLogGroups env nextToken = do
  let query = Logs.newDescribeLogGroups & describeLogGroups_nextToken .~ nextToken
  res <- AWS.send env query
  let lgs = fromMaybe [] $ res ^. describeLogGroupsResponse_logGroups
  let nextNextToken = res ^. describeLogGroupsResponse_nextToken
  case nextNextToken of
    Nothing -> pure lgs
    Just t -> do
      lgs' <- describeAllLogGroups env (Just t)
      pure $ lgs ++ lgs'

globLogGroupName :: Pattern -> Logs.LogGroup -> Bool
globLogGroupName p lg = case view logGroup_logGroupName lg of
  Nothing -> False
  Just n -> Glob.match p (Text.unpack n)

-- Query Library Management Functions

listQueries :: Maybe FilePath -> IO ()
listQueries maybeLibPath = do
  let defaultDir = "~/.ciqt/queries"
  libPath <- maybe defaultDir id <$> pure maybeLibPath
  expandedPath <- expandTilde libPath
  exists <- doesDirectoryExist expandedPath
  if not exists
    then TIO.putStrLn $ "Query library directory does not exist: " <> Text.pack expandedPath
    else do
      queries <- findQueries expandedPath expandedPath
      if null queries
        then TIO.putStrLn "No queries found in library"
        else do
          TIO.putStrLn $ "Queries in " <> Text.pack expandedPath <> ":"
          mapM_ (TIO.putStrLn . ("  " <>)) queries

findQueries :: FilePath -> FilePath -> IO [Text]
findQueries basePath currentPath = do
  contents <- listDirectory currentPath
  (dirs, files) <- partitionM (\name -> doesDirectoryExist (currentPath </> name)) contents
  let queryFiles = filter isQueryFile files
      relativePaths = map (Text.pack . makeRelative basePath . (currentPath </>)) queryFiles
      relativePathsWithoutExt = map (Text.pack . dropExtension . Text.unpack) relativePaths

  subQueries <- concat <$> mapM (findQueries basePath . (currentPath </>)) dirs
  pure $ relativePathsWithoutExt ++ subQueries
  where
    isQueryFile name = takeExtension name == ".query"

partitionM :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x : xs) = do
  result <- f x
  (trues, falses) <- partitionM f xs
  if result
    then pure (x : trues, falses)
    else pure (trues, x : falses)

saveQuery :: Maybe FilePath -> Text -> Text -> IO ()
saveQuery maybeLibPath name queryText = do
  let defaultDir = "~/.ciqt/queries"
  libPath <- maybe defaultDir id <$> pure maybeLibPath
  expandedPath <- expandTilde libPath
  let queryPath = expandedPath </> Text.unpack name <> ".query"
      queryDir = takeDirectory queryPath

  createDirectoryIfMissing True queryDir
  TIO.writeFile queryPath queryText
  TIO.putStrLn $ "Query saved as: " <> Text.pack (makeRelative expandedPath queryPath)

deleteQuery :: Maybe FilePath -> Text -> IO ()
deleteQuery maybeLibPath name = do
  let defaultDir = "~/.ciqt/queries"
  libPath <- maybe defaultDir id <$> pure maybeLibPath
  expandedPath <- expandTilde libPath
  let queryPath = expandedPath </> Text.unpack name <> ".query"

  exists <- doesFileExist queryPath
  if not exists
    then TIO.putStrLn $ "Query not found: " <> name
    else do
      removeFile queryPath
      TIO.putStrLn $ "Query deleted: " <> name

-- AWS Query Management Functions

listAWSQueries :: AWS.Env -> IO ()
listAWSQueries env = AWS.runResourceT $ do
  queries <- describeAllAWSQueries env Nothing
  liftIO $ do
    if null queries
      then TIO.putStrLn "No AWS saved queries found"
      else do
        TIO.putStrLn "AWS CloudWatch Logs Insights saved queries:"
        mapM_ printAWSQuery queries
  where
    printAWSQuery qdef = do
      let queryId = fromMaybe "<no-id>" (qdef ^. queryDefinition_queryDefinitionId)
          name = fromMaybe "<no-name>" (qdef ^. queryDefinition_name)
          logGroups = fromMaybe [] (qdef ^. queryDefinition_logGroupNames)
      TIO.putStrLn $ "  " <> queryId <> ": " <> name
      unless (null logGroups) $
        TIO.putStrLn $
          "    Log Groups: " <> Text.intercalate ", " logGroups

describeAllAWSQueries :: (MonadResource m) => AWS.Env -> Maybe Text -> m [QueryDefinition]
describeAllAWSQueries env nextToken = do
  let query = Logs.newDescribeQueryDefinitions & describeQueryDefinitions_nextToken .~ nextToken
  res <- AWS.send env query
  let queries = fromMaybe [] $ res ^. describeQueryDefinitionsResponse_queryDefinitions
  let nextNextToken = res ^. describeQueryDefinitionsResponse_nextToken
  case nextNextToken of
    Nothing -> pure queries
    Just t -> do
      moreQueries <- describeAllAWSQueries env (Just t)
      pure $ queries ++ moreQueries

getAWSQueryById :: AWS.Env -> Text -> IO Text
getAWSQueryById env queryId = AWS.runResourceT $ do
  queries <- describeAllAWSQueries env Nothing
  case filter (\q -> (q ^. queryDefinition_queryDefinitionId) == Just queryId) queries of
    [] -> liftIO $ fail $ "AWS query not found: " ++ Text.unpack queryId
    (q : _) -> case q ^. queryDefinition_queryString of
      Nothing -> liftIO $ fail $ "AWS query has no query string: " ++ Text.unpack queryId
      Just queryStr -> pure queryStr

downloadAWSQuery :: AWS.Env -> Maybe FilePath -> Text -> Text -> IO ()
downloadAWSQuery env queryLibPath queryId localName = AWS.runResourceT $ do
  queryStr <- liftIO $ getAWSQueryById env queryId
  liftIO $ saveQuery queryLibPath localName queryStr
  liftIO $ TIO.putStrLn $ "Downloaded AWS query " <> queryId <> " as: " <> localName

uploadLocalQuery :: AWS.Env -> Maybe FilePath -> Text -> IO ()
uploadLocalQuery env queryLibPath localName = do
  queryStr <- calculateQueryFromArg (QueryLibrary localName) queryLibPath
  AWS.runResourceT $ do
    let putQuery = Logs.newPutQueryDefinition (Text.unpack localName) queryStr
    res <- AWS.send env putQuery
    case res ^. putQueryDefinitionResponse_queryDefinitionId of
      Nothing -> liftIO $ TIO.putStrLn $ "Failed to upload query: " <> localName
      Just queryId -> liftIO $ TIO.putStrLn $ "Uploaded query " <> localName <> " with ID: " <> queryId

deleteAWSQuery :: AWS.Env -> Text -> IO ()
deleteAWSQuery env queryId = AWS.runResourceT $ do
  let deleteQuery = Logs.newDeleteQueryDefinition queryId
  _ <- AWS.send env deleteQuery
  liftIO $ TIO.putStrLn $ "Deleted AWS query: " <> queryId

syncQueries :: AWS.Env -> Maybe FilePath -> IO ()
syncQueries env queryLibPath = do
  TIO.putStrLn "Syncing queries between local library and AWS..."

  -- List both local and AWS queries
  localQueries <- do
    let defaultDir = "~/.ciqt/queries"
    libPath <- maybe defaultDir id <$> pure queryLibPath
    expandedPath <- expandTilde libPath
    exists <- doesDirectoryExist expandedPath
    if exists
      then findQueries expandedPath expandedPath
      else pure []

  awsQueries <- AWS.runResourceT $ describeAllAWSQueries env Nothing

  TIO.putStrLn $ "Found " <> Text.pack (show (length localQueries)) <> " local queries"
  TIO.putStrLn $ "Found " <> Text.pack (show (length awsQueries)) <> " AWS queries"

  -- For now, just list both - full sync would be more complex
  unless (null localQueries) $ do
    TIO.putStrLn "\nLocal queries:"
    mapM_ (TIO.putStrLn . ("  " <>)) localQueries

  unless (null awsQueries) $ do
    TIO.putStrLn "\nAWS queries:"
    mapM_
      ( \q -> do
          let queryId = fromMaybe "<no-id>" (q ^. queryDefinition_queryDefinitionId)
              name = fromMaybe "<no-name>" (q ^. queryDefinition_name)
          TIO.putStrLn $ "  " <> queryId <> ": " <> name
      )
      awsQueries
