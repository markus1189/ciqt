{-# LANGUAGE OverloadedStrings #-}

module Ciqt.Library
  ( -- * Query Resolution
    calculateQueryFromArg,
    
    -- * Local Library Management
    listQueries,
    saveQuery,
    deleteQuery,
  )
where

import Ciqt.AWS (getAWSQueryById, discoverAwsEnv, fastLoggerToAwsLogger)
import Ciqt.Types (QueryArg (..))
import Ciqt.Utils (expandTilde, findQueries)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath (dropExtension, makeRelative, takeDirectory, (</>))
import System.Log.FastLogger (FastLogger, LogType' (LogStderr), defaultBufSize, withFastLogger)

-- | Calculate query content from different sources
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

-- | List all queries in the local library
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

-- | Save a query to the local library
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

-- | Delete a query from the local library
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