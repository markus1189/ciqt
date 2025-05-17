{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wall #-}

import Amazonka (trying)
import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs (QueryStatus (..))
import Amazonka.CloudWatchLogs qualified as Logs
import Amazonka.CloudWatchLogs.GetQueryResults (getQueryResultsResponse_results, getQueryResultsResponse_statistics)
import Amazonka.CloudWatchLogs.Lens (describeLogGroupsResponse_logGroups, describeLogGroupsResponse_nextToken, describeLogGroups_logGroupNamePattern, describeLogGroups_logGroupNamePrefix, describeLogGroups_nextToken, getQueryResultsResponse_status, logGroup_logGroupName, resultField_field, resultField_value, startQueryResponse_queryId, startQuery_limit)
import Amazonka.CloudWatchLogs.StartQuery (startQuery_logGroupNames)
import Amazonka.CloudWatchLogs.StopQuery (newStopQuery)
import Amazonka.CloudWatchLogs.Types (QueryStatistics (QueryStatistics'), ResultField, bytesScanned, recordsMatched, recordsScanned)
import Amazonka.Prelude (mapMaybe)
import Control.Applicative ((<|>))
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
    eitherReader,
    execParser,
    fullDesc,
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
    strOption,
    switch,
    value,
    (<**>),
  )
import System.Exit (exitFailure, exitSuccess)
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

data QueryArg = QueryFile FilePath | QueryString Text deriving (Eq, Show)

data AppArgs = AppArgs
  { _appArgsQuery :: QueryArg,
    _appArgsLimit :: Maybe Limit,
    _appArgsTimeRange :: TimeRange,
    _appArgsLogGroups :: LogGroupsArg,
    _appArgsDryRun :: Bool
  }
  deriving (Show)

makeLenses ''AppArgs

appArgsParser :: Parser AppArgs
appArgsParser =
  AppArgs
    <$> (queryFileParser <|> queryStringParser)
    <*> limitParser
    <*> (timeRangeAbsolute <|> timeRangeRelative)
    <*> logGroupsParser
    <*> switch (long "dry-run" <> help "Print arguments and query, but don't start it")
  where
    timeRangeAbsolute =
      TimeRangeAbsolute
        <$> option (maybeReader (formatParseM iso8601Format)) (long "start" <> metavar "TIME")
        <*> optional (option (maybeReader (formatParseM iso8601Format)) (long "end" <> metavar "TIME"))

    timeRangeRelative =
      TimeRangeRelative
        <$> option (maybeReader (formatParseM iso8601Format)) (long "since" <> metavar "ISO8601_DURATION")

    queryFileParser =
      QueryFile
        <$> strOption
          ( long "query-file"
              <> metavar "FILE"
              <> help "Path to a file with a cloudwatch insights query"
          )

    queryStringParser =
      QueryString
        <$> strOption
          ( long "query"
              <> metavar "QUERY"
              <> help "Cloudwatch query to execute"
              <> showDefault
              <> value "fields @timestamp, @message, @logStream, @log | sort @timestamp desc"
          )
    limitParser =
      optional
        ( ExplicitLimit
            <$> option auto (long "limit" <> metavar "POSINT")
            <|> MaxLimit <$ switch (long "limit-max" <> help "Use max limit for results from AWS")
        )
    logGroupsParser =
      CommaLogGroups
        <$> option
          (maybeReader (NonEmpty.nonEmpty . map Text.pack . splitOn ","))
          ( long "log-groups" <> metavar "LOG_GROUPS" <> help "Comma separated list of cloudwatch log groups to search"
          )
        <|> LogNamePattern
          <$> strOption
            ( long "log-group-pattern" <> metavar "LOG_GROUP_PATTERN" <> help "Match log groups based on case-sensitive substring search"
            )
        <|> LogNamePrefix
          <$> strOption
            ( long "log-group-prefix" <> metavar "LOG_GROUP_PREFIX" <> help "Match log groups based on case-sensitive prefix search"
            )
        <|> LogNameGlob
          <$> option
            (eitherReader (\t -> mapLeft (\err -> "Could not compile glob: " +| t |+ ". Error was: " +| err |+ "") . tryCompileWith compDefault $ t))
            ( long "log-group-glob" <> metavar "LOG_GROUP_GLOB" <> help "Retrieve ALL LOG GROUPS (!) and then match via glob pattern"
            )
        <|> LogNameRegex
          <$> option
            (eitherReader (\t -> fmap (Text.pack t,) . mapLeft (\err -> "Could not parse regex: " +| t |+ ". Error was: " +| err |+ "") . runIdentity . runFailT . makeRegexM @Regex @_ @_ @_ @(FailT String Identity) $ t))
            ( long "log-group-regex" <> metavar "LOG_GROUP_REGEX" <> help "Retrieve ALL LOG GROUPS (!) and then match via PCRE regex"
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
  let opts = info (appArgsParser <**> helper) (fullDesc <> progDesc "Execute cloudwatch insights log queries")
  appArgs <- execParser opts

  tz <- getCurrentTimeZone
  now <- liftIO getCurrentTime
  env <- discoverAwsEnv fastLogger

  maybeLgs <- calculateLogGroups env (appArgs ^. appArgsLogGroups)
  lgs <- case maybeLgs of
    Nothing -> do
      fastLogger . toLogStr @Text $ "Could not find any log groups from " +| show (appArgs ^. appArgsLogGroups) |+ ""
      exitFailure
    Just lgs ->
      pure lgs

  let (queryStart, queryEnd) = calculateQueryStartEnd now appArgs
      limit = calculateLimit appArgs

  query <- calculateQuery appArgs

  let awsQuery = buildQuery limit lgs queryStart queryEnd query

  printPreFlightInfo env tz (queryStart, queryEnd) lgs limit query

  unless (appArgs ^. appArgsDryRun) . AWS.runResourceT $ do
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

calculateQueryStartEnd :: UTCTime -> AppArgs -> (UTCTime, UTCTime)
calculateQueryStartEnd now appArgs =
  case appArgs ^. appArgsTimeRange of
    TimeRangeAbsolute s e -> (zonedTimeToUTC s, maybe now zonedTimeToUTC e)
    TimeRangeRelative d -> (addUTCTime (negate . fromInteger @NominalDiffTime . read $ formatTime defaultTimeLocale "%s" d) now, now)

calculateLimit :: AppArgs -> Maybe Natural
calculateLimit appArgs =
  appArgs ^. appArgsLimit <&> \case
    MaxLimit -> 10000
    ExplicitLimit x -> x

calculateQuery :: AppArgs -> IO Text
calculateQuery appArgs = case appArgs ^. appArgsQuery of
  QueryString q -> pure q
  QueryFile f -> TIO.readFile f

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
