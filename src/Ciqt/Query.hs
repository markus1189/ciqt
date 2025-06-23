{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Ciqt.Query
  ( -- * Query Construction and Execution
    buildQuery,
    executeQuery,
    
    -- * Time and Parameter Calculations
    calculateQueryStartEnd,
    calculateLimitFromRunArgs,
    calculateQueryFromRunArgs,
  )
where

import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs (QueryStatus (..))
import Amazonka.CloudWatchLogs qualified as Logs
import Amazonka.CloudWatchLogs.GetQueryResults (getQueryResultsResponse_results, getQueryResultsResponse_statistics)
import Amazonka.CloudWatchLogs.Lens (getQueryResultsResponse_status, startQueryResponse_queryId, startQuery_limit)
import Amazonka.CloudWatchLogs.StartQuery (startQuery_logGroupNames)
import Amazonka.CloudWatchLogs.StopQuery (newStopQuery)
import Ciqt.Library (calculateQueryFromArg)
import Ciqt.Types (Limit (..), RunArgs (..), TimeRange (..))
import Ciqt.Utils (formatQueryStats)
import Control.Exception.Lens (trying)
import Control.Lens (view, (.~), (?~), (^.), (&))
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Control.Retry (constantDelay, limitRetriesByCumulativeDelay, retrying)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (CalendarDiffTime, NominalDiffTime, UTCTime, ZonedTime, addUTCTime, formatTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale)
import Data.Traversable (for)
import Fmt ((+|), (|+))
import NeatInterpolation (trimming)
import Numeric.Natural (Natural)
import System.IO (stderr)

-- | Build AWS StartQuery from parameters
buildQuery :: Maybe Natural -> NonEmpty Text -> UTCTime -> UTCTime -> Text -> Logs.StartQuery
buildQuery n lgs qstart qend q =
  Logs.newStartQuery (toSeconds qstart) (toSeconds qend) q
    & startQuery_logGroupNames ?~ NonEmpty.toList lgs
    & startQuery_limit .~ n
  where
    toSeconds = round . utcTimeToPOSIXSeconds

-- | Execute query with resource management and polling
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
                Right _ -> liftIO $ TIO.hPutStrLn stderr [trimming|Succeeded stopping query: $queryId|]
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

-- | Calculate query start and end times from time range
calculateQueryStartEnd :: UTCTime -> TimeRange -> (UTCTime, UTCTime)
calculateQueryStartEnd now timeRange =
  case timeRange of
    TimeRangeAbsolute s e -> (zonedTimeToUTC s, maybe now zonedTimeToUTC e)
    TimeRangeRelative d -> (addUTCTime (negate . fromInteger @NominalDiffTime . read $ formatTime defaultTimeLocale "%s" d) now, now)

-- | Calculate limit from run arguments
calculateLimitFromRunArgs :: RunArgs -> Maybe Natural
calculateLimitFromRunArgs runArgs =
  case _runArgsLimit runArgs of
    Nothing -> Nothing
    Just MaxLimit -> Just 10000
    Just (ExplicitLimit x) -> Just x

-- | Calculate query from run arguments
calculateQueryFromRunArgs :: RunArgs -> Maybe FilePath -> IO Text
calculateQueryFromRunArgs runArgs queryLibPath =
  calculateQueryFromArg (_runArgsQuery runArgs) queryLibPath