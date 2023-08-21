{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

import Amazonka (trying)
import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs (QueryStatus (..))
import Amazonka.CloudWatchLogs qualified as Logs
import Amazonka.CloudWatchLogs.GetQueryResults (getQueryResultsResponse_results, getQueryResultsResponse_statistics)
import Amazonka.CloudWatchLogs.Lens (getQueryResultsResponse_status, resultField_field, resultField_value, startQueryResponse_queryId, startQuery_limit)
import Amazonka.CloudWatchLogs.StartQuery (startQuery_logGroupNames)
import Amazonka.CloudWatchLogs.StopQuery (newStopQuery)
import Amazonka.CloudWatchLogs.Types (QueryStatistics (QueryStatistics'), ResultField, bytesScanned, recordsMatched, recordsScanned)
import Amazonka.Prelude (mapMaybe)
import Control.Applicative ((<|>))
import Control.Lens (view, (%~))
import Control.Lens.Operators ((&), (.~), (<&>), (?~), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (unless)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Retry (constantDelay, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (CalendarDiffTime, NominalDiffTime, UTCTime, ZonedTime, addUTCTime, diffUTCTime, formatTime, getCurrentTime, getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM)
import Data.Traversable (for)
import Fmt (build, commaizeF, dateDashF, hmsF, listF, padLeftF, padRightF, tzF, unwordsF, (+|), (|+), indentF)
import Formatting (sformat, (%), (%.))
import Formatting.Combinators qualified as Formatters
import Formatting.Formatters qualified as Formatters
import NeatInterpolation (trimming)
import Numeric.Natural (Natural)
import Options.Applicative
  ( Parser,
    auto,
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
import System.IO (hFlush, stderr, stdout)
import System.IO qualified as IO

data TimeRange = TimeRangeAbsolute ZonedTime (Maybe ZonedTime) | TimeRangeRelative CalendarDiffTime deriving (Show)

data Limit = ExplicitLimit Natural | MaxLimit deriving (Eq, Show)

data AppArgs = AppArgs
  { _appArgsQuery :: Text,
    _appArgsLimit :: Maybe Limit,
    _appArgsTimeRange :: TimeRange,
    _appArgsLogGroups :: NonEmpty Text,
    _appArgsDryRun :: Bool
  }
  deriving (Show)

makeLenses ''AppArgs

appArgsParser :: Parser AppArgs
appArgsParser =
  AppArgs
    <$> strOption
      ( long "query"
          <> metavar "QUERY"
          <> help "Cloudwatch query to execute"
          <> showDefault
          <> value "fields @timestamp, @message, @logStream, @log | sort @timestamp desc"
      )
    <*> optional (ExplicitLimit <$> option auto (long "limit" <> metavar "POSINT") <|> MaxLimit <$ switch (long "limit-max" <> help "Use max limit for results from AWS"))
    <*> (timeRangeAbsolute <|> timeRangeRelative)
    <*> option (maybeReader (NonEmpty.nonEmpty . map Text.pack . splitOn ",")) (long "log-groups" <> metavar "LOG_GROUPS" <> help "Comma separated list of cloudwatch log groups to search")
    <*> switch (long "dry-run" <> help "Print arguments and query, but don't start it")
  where
    timeRangeAbsolute =
      TimeRangeAbsolute
        <$> option (maybeReader (formatParseM iso8601Format)) (long "start" <> metavar "TIME")
        <*> optional (option (maybeReader (formatParseM iso8601Format)) (long "end" <> metavar "TIME"))
    timeRangeRelative = TimeRangeRelative <$> option (maybeReader (formatParseM iso8601Format)) (long "since" <> metavar "ISO8601_DURATION")

main :: IO ()
main = do
  let opts = info (appArgsParser <**> helper) (fullDesc <> progDesc "Execute cloudwatch logs insight queries")
  appArgs <- execParser opts
  logger <- AWS.newLogger AWS.Error IO.stdout

  tz <- getCurrentTimeZone

  discoveredEnv <- AWS.newEnv AWS.discover

  now <- liftIO getCurrentTime

  let env =
        discoveredEnv
          { AWS.logger = logger,
            AWS.region = AWS.Frankfurt
          }
      (queryStart, queryEnd) = case appArgs ^. appArgsTimeRange of
        TimeRangeAbsolute s e -> (zonedTimeToUTC s, maybe now zonedTimeToUTC e)
        TimeRangeRelative d -> (addUTCTime (negate . fromInteger @NominalDiffTime . read $ formatTime defaultTimeLocale "%s" d) now, now)
      limit =
        appArgs ^. appArgsLimit <&> \case
          MaxLimit -> 10000
          ExplicitLimit x -> x

      query = appArgs ^. appArgsQuery

      errorsQuery = buildQuery limit (appArgs ^. appArgsLogGroups) queryStart queryEnd query

  TIO.hPutStrLn
    stderr
    ( Text.intercalate
        "\n"
        [ padRightF @Text 14 ' ' "Query Start: " +| dateDashF (utcToZonedTime tz queryStart) |+ "T" +| hmsF (utcToZonedTime tz queryStart) |+ tzF (utcToZonedTime tz queryStart),
          padRightF @Text 14 ' ' "Query End: " +| dateDashF (utcToZonedTime tz queryEnd) |+ "T" +| hmsF (utcToZonedTime tz queryEnd) |+ tzF (utcToZonedTime tz queryEnd),
          padRightF @Text 14 ' ' "Interval: " +| Text.pack (formatTime defaultTimeLocale "%d days, %H hours, %M minutes, %S seconds" $ diffUTCTime queryEnd queryStart) |+ "",
          padRightF @Text 14 ' ' "Limit: " +| maybe "<no-limit-specified>" (commaizeF . fromIntegral @Natural @Integer) limit |+ "",
          padRightF @Text 14 ' ' "LogGroups: " +| unwordsF (appArgs ^. appArgsLogGroups) |+ "",
          "Query:\n\n" +| indentF 2 (build query),
          "---------------------------"
        ]
    )
  unless (appArgs ^. appArgsDryRun) $ AWS.runResourceT $ do
    mresult <- executeQuery env errorsQuery
    liftIO $ TIO.hPutStrLn stderr "---------------------------"
    for_ mresult $ \result -> do
      let mlogEvents = result ^. getQueryResultsResponse_results
          stats = result ^. getQueryResultsResponse_statistics
      for_ mlogEvents $ \logEvents -> do
        for_ logEvents $ \logEvent -> do
          let logEvent' = resultFieldsToJson logEvent & key "@message" %~ parseNestedJson
          liftIO . LBS.hPutStrLn stdout . Aeson.encode $ logEvent'

      liftIO $ do
        hFlush stdout
        TIO.hPutStrLn stderr $ "---------------------------\nStatistics:\n" +| fmap formatQueryStats stats |+ "\n---------------------------"

  hFlush stdout >> hFlush stderr

buildQuery :: Maybe Natural -> NonEmpty Text -> UTCTime -> UTCTime -> Text -> Logs.StartQuery
buildQuery n lgs qstart qend q =
  Logs.newStartQuery (toSeconds qstart) (toSeconds qend) q
    & startQuery_logGroupNames ?~ NonEmpty.toList lgs
    & startQuery_limit .~ n
  where
    toSeconds = round . utcTimeToPOSIXSeconds

parseNestedJson :: Value -> Value
parseNestedJson v@(Aeson.String s) = fromMaybe v (Aeson.decode @Value $ toLazyByteString $ Text.encodeUtf8Builder s)
parseNestedJson v = v

resultFieldsToJson :: [ResultField] -> Aeson.Value
resultFieldsToJson rfs = Aeson.toJSON $ Map.fromList $ mapMaybe resultFieldToJson rfs

resultFieldToJson :: ResultField -> Maybe (Text, Text)
resultFieldToJson rf = (,) <$> view resultField_field rf <*> view resultField_value rf

executeQuery env query = do
  bracket
    (AWS.send env query)
    ( \x -> do
        let qid = x ^. startQueryResponse_queryId
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
    )
    $ \res -> do
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
      sformat ("Bytes Scanned: " % (Formatters.left 13 ' ' %. Formatters.maybed "" (Formatters.bytes (Formatters.fixed 2)))) (fmap round bytesS)
    ]
