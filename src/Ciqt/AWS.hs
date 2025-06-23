{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ciqt.AWS
  ( -- * Environment Setup
    discoverAwsEnv,
    fastLoggerToAwsLogger,
    
    -- * Log Group Operations
    calculateLogGroups,
    describeAllLogGroups,
    globLogGroupName,
    
    -- * AWS Query Management
    listAWSQueries,
    describeAllAWSQueries,
    getAWSQueryById,
    downloadAWSQuery,
    uploadLocalQuery,
    deleteAWSQuery,
    syncQueries,
  )
where

import Amazonka qualified as AWS
import Amazonka.CloudWatchLogs qualified as Logs
import Amazonka.CloudWatchLogs.Lens 
  ( describeLogGroupsResponse_logGroups,
    describeLogGroupsResponse_nextToken,
    describeLogGroups_logGroupNamePattern,
    describeLogGroups_logGroupNamePrefix,
    describeLogGroups_nextToken,
    describeQueryDefinitionsResponse_nextToken,
    describeQueryDefinitionsResponse_queryDefinitions,
    describeQueryDefinitions_nextToken,
    logGroup_logGroupName,
    putQueryDefinitionResponse_queryDefinitionId,
    queryDefinition_logGroupNames,
    queryDefinition_name,
    queryDefinition_queryDefinitionId,
    queryDefinition_queryString,
  )
import Amazonka.CloudWatchLogs.Types (QueryDefinition)
import Amazonka.Prelude (mapMaybe)
import Ciqt.Types (LogGroupsArg (..), QueryArg (..))
import Control.Lens (view, (.~), (?~), (^.), (&))
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.FilePath (takeExtension, dropExtension, makeRelative, (</>))
import System.FilePath.Glob (Pattern)
import System.FilePath.Glob qualified as Glob
import System.Log.FastLogger (FastLogger, ToLogStr (toLogStr))
import Text.Regex.PCRE (Regex, RegexContext (match))

-- | Convert FastLogger to AWS Logger format
fastLoggerToAwsLogger :: FastLogger -> AWS.Logger
fastLoggerToAwsLogger fastLogger lvl bss = 
  when (lvl <= AWS.Error) $ fastLogger (toLogStr bss)
  where
    when condition action = if condition then action else pure ()

-- | Discover and configure AWS environment with logging
discoverAwsEnv :: (MonadIO m, MonadCatch m) => FastLogger -> m AWS.Env
discoverAwsEnv logger = do
  env <- AWS.newEnv AWS.discover
  pure $ env {AWS.logger = fastLoggerToAwsLogger logger}

-- | Calculate log groups from different selection methods
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

-- | Fetch all log groups with pagination support
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

-- | Check if log group name matches glob pattern
globLogGroupName :: Pattern -> Logs.LogGroup -> Bool
globLogGroupName p lg = case view logGroup_logGroupName lg of
  Nothing -> False
  Just n -> Glob.match p (Text.unpack n)

-- | List all AWS saved queries
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

-- | Fetch all AWS query definitions with pagination
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

-- | Get AWS query content by ID
getAWSQueryById :: AWS.Env -> Text -> IO Text
getAWSQueryById env queryId = AWS.runResourceT $ do
  queries <- describeAllAWSQueries env Nothing
  case filter (\q -> (q ^. queryDefinition_queryDefinitionId) == Just queryId) queries of
    [] -> liftIO $ fail $ "AWS query not found: " ++ Text.unpack queryId
    (q : _) -> case q ^. queryDefinition_queryString of
      Nothing -> liftIO $ fail $ "AWS query has no query string: " ++ Text.unpack queryId
      Just queryStr -> pure queryStr

-- | Download AWS query to local library
-- Note: Implementation moved to main module to avoid circular dependency
downloadAWSQuery :: AWS.Env -> Maybe FilePath -> Text -> Text -> IO ()
downloadAWSQuery env queryLibPath queryId localName = do
  TIO.putStrLn $ "Download functionality temporarily disabled - use ciqt main module"

-- | Upload local query to AWS
-- Note: Implementation moved to main module to avoid circular dependency
uploadLocalQuery :: AWS.Env -> Maybe FilePath -> Text -> IO ()
uploadLocalQuery env queryLibPath localName = do
  TIO.putStrLn $ "Upload functionality temporarily disabled - use ciqt main module"

-- | Delete AWS saved query
deleteAWSQuery :: AWS.Env -> Text -> IO ()
deleteAWSQuery env queryId = AWS.runResourceT $ do
  let deleteQuery = Logs.newDeleteQueryDefinition queryId
  _ <- AWS.send env deleteQuery
  liftIO $ TIO.putStrLn $ "Deleted AWS query: " <> queryId

-- | Sync queries between local library and AWS
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

-- Helper functions - TODO: move to Ciqt.Utils
expandTilde :: FilePath -> IO FilePath
expandTilde path = case path of
  '~' : '/' : rest -> do
    home <- getHomeDirectory
    return $ home </> rest
  _ -> return path

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