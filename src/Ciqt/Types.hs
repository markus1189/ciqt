{-# LANGUAGE TemplateHaskell #-}

module Ciqt.Types
  ( -- * Data Types
    LogGroupsArg (..),
    TimeRange (..),
    Limit (..),
    QueryArg (..),
    RunArgs (..),
    LibraryOperation (..),
    LibraryArgs (..),
    QueryShowArgs (..),
    Command (..),
    AppArgs (..),
    
    -- * Lenses
    runArgsQuery,
    runArgsLimit,
    runArgsTimeRange,
    runArgsLogGroups,
    runArgsDryRun,
    runArgsQueryLibrary,
    libraryArgsOperation,
    libraryArgsQueryLibrary,
    queryShowArgsQuery,
    queryShowArgsQueryLibrary,
    appArgsCommand,
    appArgsGlobalQueryLibrary,
  )
where

import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Fmt ((+|), (|+))
import Numeric.Natural (Natural)
import System.FilePath.Glob (Pattern)
import Text.Regex.PCRE (Regex)

-- | Different methods for specifying log groups
data LogGroupsArg
  = CommaLogGroups (NonEmpty Text) -- ^ Comma-separated list of log group names
  | LogNamePattern Text            -- ^ Substring pattern matching
  | LogNamePrefix Text             -- ^ Prefix-based matching
  | LogNameGlob Pattern           -- ^ Shell glob pattern matching
  | LogNameRegex (Text, Regex)    -- ^ PCRE regex pattern matching

instance Show LogGroupsArg where
  show (CommaLogGroups lgs) = "CommaLogGroups " +| show lgs |+ ""
  show (LogNamePattern p) = "LogNamePattern " +| show p |+ ""
  show (LogNamePrefix s) = "LogNamePrefix " +| show s |+ ""
  show (LogNameGlob g) = "LogNameGlob " +| show g |+ ""
  show (LogNameRegex (r, _)) = "LogNameRegex " +| show r |+ ""

-- | Time range specification for queries
data TimeRange 
  = TimeRangeAbsolute ZonedTime (Maybe ZonedTime) -- ^ Absolute start and optional end time
  | TimeRangeRelative CalendarDiffTime            -- ^ Relative time range (e.g., last hour)
  deriving (Show)

-- | Query result limit specification
data Limit 
  = ExplicitLimit Natural -- ^ Specific numeric limit
  | MaxLimit              -- ^ Use AWS maximum limit (10,000)
  deriving (Eq, Show)

-- | Different sources for query content
data QueryArg 
  = QueryFile FilePath    -- ^ Read query from file
  | QueryString Text      -- ^ Query provided as string
  | QueryLibrary Text     -- ^ Query from local library by name
  | QueryAWS Text         -- ^ Query from AWS by ID
  deriving (Eq, Show)

-- | Arguments for the run command
data RunArgs = RunArgs
  { _runArgsQuery :: QueryArg,           -- ^ Query source specification
    _runArgsLimit :: Maybe Limit,        -- ^ Optional result limit
    _runArgsTimeRange :: Maybe TimeRange, -- ^ Optional time range
    _runArgsLogGroups :: Maybe LogGroupsArg, -- ^ Optional log groups specification
    _runArgsDryRun :: Bool,              -- ^ Whether to perform dry run only
    _runArgsQueryLibrary :: Maybe FilePath -- ^ Optional custom query library path
  }
  deriving (Show)

-- | Library management operations
data LibraryOperation
  = ListQueries           -- ^ List all queries in library
  | SaveQuery Text QueryArg -- ^ Save query to library
  | DeleteQuery Text      -- ^ Delete query from library
  | ShowQuery Text        -- ^ Display query content
  | ListAWSQueries        -- ^ List AWS saved queries
  | DownloadAWSQuery Text Text -- ^ Download AWS query to local (AWS ID, local name)
  | UploadQuery Text      -- ^ Upload local query to AWS
  | DeleteAWSQuery Text   -- ^ Delete AWS saved query
  | SyncQueries           -- ^ Sync between local and AWS
  deriving (Show)

-- | Arguments for the library command
data LibraryArgs = LibraryArgs
  { _libraryArgsOperation :: LibraryOperation, -- ^ Operation to perform
    _libraryArgsQueryLibrary :: Maybe FilePath  -- ^ Optional custom library path
  }
  deriving (Show)

-- | Arguments for the query show command
data QueryShowArgs = QueryShowArgs
  { _queryShowArgsQuery :: QueryArg,           -- ^ Query to display
    _queryShowArgsQueryLibrary :: Maybe FilePath -- ^ Optional custom library path
  }
  deriving (Show)

-- | Top-level commands
data Command
  = RunCommand RunArgs           -- ^ Execute query command
  | LibraryCommand LibraryArgs   -- ^ Library management command
  | QueryShowCommand QueryShowArgs -- ^ Query display command
  deriving (Show)

-- | Top-level application arguments
data AppArgs = AppArgs
  { _appArgsCommand :: Command,              -- ^ Command to execute
    _appArgsGlobalQueryLibrary :: Maybe FilePath -- ^ Global query library path
  }
  deriving (Show)

-- Generate lenses for all data types
makeLenses ''RunArgs
makeLenses ''LibraryArgs
makeLenses ''QueryShowArgs
makeLenses ''AppArgs