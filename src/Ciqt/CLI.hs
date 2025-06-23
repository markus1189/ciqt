{-# LANGUAGE OverloadedStrings #-}

module Ciqt.CLI
  ( -- * Parser Functions
    appArgsParser,
    parseAppArgs,
  )
where

import Ciqt.Types
import Control.Applicative ((<|>))
import Data.Either.Combinators (mapLeft)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split (splitOn)
import Data.Text qualified as Text
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM)
import Fmt ((+|), (|+))
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
import System.FilePath.Glob (compDefault, tryCompileWith)
import Text.Regex.PCRE (Regex, RegexMaker (makeRegexM))
import Control.Monad.Trans.Fail (FailT, runFailT)

-- | Top-level application arguments parser
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

-- | Command parser with subcommands and backward compatibility
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

-- | Parser for run command arguments
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

-- | Parser for library command arguments
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

-- | Parser for library operations
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

-- | Parser for save query operation
saveQueryParser :: Parser LibraryOperation
saveQueryParser =
  SaveQuery
    <$> strArgument (metavar "NAME" <> help "Name/path to save the query as (e.g., 'aws/lambda/errors')")
    <*> queryArgParser

-- | Parser for delete query operation
deleteQueryParser :: Parser LibraryOperation
deleteQueryParser =
  DeleteQuery
    <$> strArgument (metavar "NAME" <> help "Name/path of query to delete (e.g., 'aws/lambda/errors')")

-- | Parser for show query operation
showQueryParser :: Parser LibraryOperation
showQueryParser =
  ShowQuery
    <$> strArgument (metavar "NAME" <> help "Name/path of query to display (e.g., 'aws/lambda/errors')")

-- | Parser for download AWS query operation
downloadAWSQueryParser :: Parser LibraryOperation
downloadAWSQueryParser =
  DownloadAWSQuery
    <$> strArgument (metavar "QUERY_ID" <> help "AWS query definition ID to download")
    <*> strArgument (metavar "LOCAL_NAME" <> help "Local name to save the query as (e.g., 'aws/lambda/errors')")

-- | Parser for upload query operation
uploadQueryParser :: Parser LibraryOperation
uploadQueryParser =
  UploadQuery
    <$> strArgument (metavar "LOCAL_NAME" <> help "Local query name to upload to AWS (e.g., 'aws/lambda/errors')")

-- | Parser for delete AWS query operation
deleteAWSQueryParser :: Parser LibraryOperation
deleteAWSQueryParser =
  DeleteAWSQuery
    <$> strArgument (metavar "QUERY_ID" <> help "AWS query definition ID to delete")

-- | Parser for query show command arguments
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

-- | Parser for query argument with multiple sources
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

-- | Parser for time range arguments
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

-- | Parser for limit arguments
limitParser :: Parser (Maybe Limit)
limitParser =
  optional
    ( ExplicitLimit
        <$> option auto (long "limit" <> metavar "POSINT" <> help "Maximum number of log entries to return")
        <|> MaxLimit <$ switch (long "limit-max" <> help "Use maximum limit (10,000) for results from AWS")
    )

-- | Parser for log groups arguments with multiple selection methods
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

-- | Parse application arguments with help and metadata
parseAppArgs :: IO AppArgs
parseAppArgs = do
  let opts =
        info
          (appArgsParser <**> helper)
          ( fullDesc
              <> progDesc "Execute and manage CloudWatch Insights queries with flexible log group selection and time ranges"
              <> header "ciqt - CloudWatch Insights Query Tool"
              <> footer "Examples:\n  ciqt run --query 'fields @timestamp, @message | limit 10' --log-groups '/aws/lambda/my-function' --start 2023-01-01T00:00:00Z\n  ciqt library list\n  ciqt library save my-query --query 'fields @timestamp | limit 100'"
          )
  execParser opts