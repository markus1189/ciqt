{-# LANGUAGE OverloadedStrings #-}

module Ciqt.Utils
  ( -- * Path Utilities
    expandTilde,
    
    -- * File System Utilities
    findQueries,
    partitionM,
    
    -- * JSON Processing
    parseNestedJson,
    resultFieldsToJson,
    resultFieldToJson,
    
    -- * Display Utilities
    printPreFlightInfo,
    formatQueryStats,
  )
where

import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs.Types (QueryStatistics (QueryStatistics'), ResultField, bytesScanned, recordsMatched, recordsScanned)
import Amazonka.CloudWatchLogs.Lens (resultField_field, resultField_value)
import Amazonka.Prelude (mapMaybe)
import Control.Lens (view)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (toLazyByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (TimeZone, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, utcToZonedTime)
import Fmt (blockListF, build, commaizeF, dateDashF, hmsF, indentF, padLeftF, padRightF, tzF, (+|), (|+))
import Formatting (sformat, (%), (%.))
import Formatting.Combinators qualified as Formatters
import Formatting.Formatters qualified as Formatters
import Numeric.Natural (Natural)
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.FilePath (dropExtension, makeRelative, takeExtension, (</>))
import System.IO (stderr)

-- | Expand ~ in file paths to the user's home directory
expandTilde :: FilePath -> IO FilePath
expandTilde path = case path of
  '~' : '/' : rest -> do
    home <- getHomeDirectory
    return $ home </> rest
  _ -> return path

-- | Find all query files in a directory tree
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

-- | Partition a list based on a monadic predicate
partitionM :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x : xs) = do
  result <- f x
  (trues, falses) <- partitionM f xs
  if result
    then pure (x : trues, falses)
    else pure (trues, x : falses)

-- | Parse nested JSON strings in log values
parseNestedJson :: Value -> Value
parseNestedJson v@(Aeson.String s) = 
  fromMaybe v (Aeson.decode @Value $ toLazyByteString $ Text.encodeUtf8Builder s)
parseNestedJson v = v

-- | Convert AWS result fields to JSON object
resultFieldsToJson :: [ResultField] -> Aeson.Value
resultFieldsToJson rfs = Aeson.toJSON $ Map.fromList $ mapMaybe resultFieldToJson rfs

-- | Convert a single result field to key-value pair
resultFieldToJson :: ResultField -> Maybe (Text, Text)
resultFieldToJson rf = (,) <$> view resultField_field rf <*> view resultField_value rf

-- | Display pre-flight information before query execution
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

-- | Format query statistics for display
formatQueryStats :: QueryStatistics -> Text
formatQueryStats QueryStatistics' {recordsScanned = scanned, bytesScanned = bytesS, recordsMatched = matched} =
  Text.intercalate
    " / "
    [ "Records matched: " +| padLeftF 13 ' ' (commaizeF @Integer . round <$> matched),
      "Records scanned: " +| padLeftF 13 ' ' (commaizeF @Integer . round <$> scanned),
      sformat ("Bytes Scanned: " % (Formatters.left 13 ' ' %. Formatters.maybed "" (Formatters.bytes @Double @Integer (Formatters.fixed 2)))) (fmap round bytesS)
    ]