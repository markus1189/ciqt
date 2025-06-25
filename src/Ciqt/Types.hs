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
    ExecutionStatus (..),
    HistoryEntry (..),
    HistoryOperation (..),
    HistoryArgs (..),
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
    historyId,
    historyTimestamp,
    historyQuery,
    historyLogGroups,
    historyTimeRange,
    historyLimit,
    historyQueryLibrary,
    historyExecutionTime,
    historyStatus,
    historyArgsOperation,
    historyArgsHistoryDir,
    appArgsCommand,
    appArgsGlobalQueryLibrary,
    appArgsGlobalHistoryDir,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (CalendarDiffTime, NominalDiffTime, UTCTime, ZonedTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatParseM)
import Fmt ((+|), (|+))
import Numeric.Natural (Natural)
import System.FilePath.Glob (Pattern, compDefault, tryCompileWith)
import Text.Regex.PCRE (Regex, RegexMaker (makeRegexM))

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

instance Eq LogGroupsArg where
  CommaLogGroups lgs1 == CommaLogGroups lgs2 = lgs1 == lgs2
  LogNamePattern p1 == LogNamePattern p2 = p1 == p2
  LogNamePrefix s1 == LogNamePrefix s2 = s1 == s2
  LogNameGlob g1 == LogNameGlob g2 = show g1 == show g2  -- Compare string representation
  LogNameRegex (r1, _) == LogNameRegex (r2, _) = r1 == r2  -- Compare pattern text
  _ == _ = False

-- | Time range specification for queries
data TimeRange 
  = TimeRangeAbsolute ZonedTime (Maybe ZonedTime) -- ^ Absolute start and optional end time
  | TimeRangeRelative CalendarDiffTime            -- ^ Relative time range (e.g., last hour)
  deriving (Show)

instance Eq TimeRange where
  TimeRangeAbsolute s1 e1 == TimeRangeAbsolute s2 e2 = 
    show s1 == show s2 && show e1 == show e2  -- Compare string representations
  TimeRangeRelative d1 == TimeRangeRelative d2 = d1 == d2
  _ == _ = False

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

-- | Query execution status
data ExecutionStatus
  = Success  -- ^ Query executed successfully
  | Failed   -- ^ Query execution failed
  | DryRun   -- ^ Dry run mode (query not executed)
  deriving (Eq, Show)

-- | History entry representing a single query execution
data HistoryEntry = HistoryEntry
  { _historyId :: Text                        -- ^ 8-char SHA256 hash
  , _historyTimestamp :: UTCTime              -- ^ When query was executed
  , _historyQuery :: Text                     -- ^ The query text
  , _historyLogGroups :: LogGroupsArg         -- ^ Log groups specification
  , _historyTimeRange :: TimeRange            -- ^ Time range specification
  , _historyLimit :: Maybe Limit              -- ^ Result limit
  , _historyQueryLibrary :: Maybe FilePath    -- ^ Query library path used
  , _historyExecutionTime :: Maybe NominalDiffTime -- ^ How long execution took
  , _historyStatus :: ExecutionStatus         -- ^ Success/Failed/DryRun
  }
  deriving (Eq, Show)

instance Ord HistoryEntry where
  compare e1 e2 = compare (_historyTimestamp e1) (_historyTimestamp e2)

-- | History management operations
data HistoryOperation
  = ListHistory           -- ^ List all history entries
  | ShowHistory Text      -- ^ Show details of specific entry by hash
  | RerunHistory Text     -- ^ Re-execute query from history by hash
  | ClearHistory          -- ^ Clear all history entries
  deriving (Show)

-- | Arguments for the history command
data HistoryArgs = HistoryArgs
  { _historyArgsOperation :: HistoryOperation  -- ^ Operation to perform
  , _historyArgsHistoryDir :: Maybe FilePath   -- ^ Optional custom history directory
  }
  deriving (Show)

-- | Top-level commands
data Command
  = RunCommand RunArgs           -- ^ Execute query command
  | LibraryCommand LibraryArgs   -- ^ Library management command
  | QueryShowCommand QueryShowArgs -- ^ Query display command
  | HistoryCommand HistoryArgs   -- ^ History management command
  deriving (Show)

-- | Top-level application arguments
data AppArgs = AppArgs
  { _appArgsCommand :: Command,              -- ^ Command to execute
    _appArgsGlobalQueryLibrary :: Maybe FilePath, -- ^ Global query library path
    _appArgsGlobalHistoryDir :: Maybe FilePath -- ^ Global history directory path
  }
  deriving (Show)

-- JSON instances for dependent types
instance ToJSON LogGroupsArg where
  toJSON (CommaLogGroups lgs) = object ["CommaLogGroups" .= lgs]
  toJSON (LogNamePattern p) = object ["LogNamePattern" .= p]
  toJSON (LogNamePrefix s) = object ["LogNamePrefix" .= s]
  toJSON (LogNameGlob g) = object ["LogNameGlob" .= show g]
  toJSON (LogNameRegex (r, _)) = object ["LogNameRegex" .= r]

instance FromJSON LogGroupsArg where
  parseJSON = withObject "LogGroupsArg" $ \o -> do
    let keys = KeyMap.keys o
    case keys of
      ["CommaLogGroups"] -> CommaLogGroups <$> o .: "CommaLogGroups"
      ["LogNamePattern"] -> LogNamePattern <$> o .: "LogNamePattern"
      ["LogNamePrefix"] -> LogNamePrefix <$> o .: "LogNamePrefix"
      ["LogNameGlob"] -> do
        globStr <- o .: "LogNameGlob"
        case tryCompileWith compDefault globStr of
          Right pattern -> pure $ LogNameGlob pattern
          Left err -> fail $ "Invalid glob pattern: " ++ err
      ["LogNameRegex"] -> do
        regexStr <- o .: "LogNameRegex"
        regex <- makeRegexM (Text.unpack regexStr)
        pure $ LogNameRegex (regexStr, regex)
      _ -> fail "Invalid LogGroupsArg"

instance ToJSON TimeRange where
  toJSON (TimeRangeAbsolute start end) = object
    [ "TimeRangeAbsolute" .= object
        [ "start" .= start
        , "end" .= end
        ]
    ]
  toJSON (TimeRangeRelative diff) = object
    [ "TimeRangeRelative" .= show diff
    ]

instance FromJSON TimeRange where
  parseJSON = withObject "TimeRange" $ \o -> do
    let keys = KeyMap.keys o
    case keys of
      ["TimeRangeAbsolute"] -> do
        absObj <- o .: "TimeRangeAbsolute"
        TimeRangeAbsolute <$> absObj .: "start" <*> absObj .: "end"
      ["TimeRangeRelative"] -> do
        diffStr <- o .: "TimeRangeRelative"
        case formatParseM iso8601Format diffStr of
          Just diff -> pure $ TimeRangeRelative diff
          Nothing -> fail "Invalid ISO8601 duration"
      _ -> fail "Invalid TimeRange"

instance ToJSON Limit where
  toJSON (ExplicitLimit n) = object ["ExplicitLimit" .= n]
  toJSON MaxLimit = object ["MaxLimit" .= True]

instance FromJSON Limit where
  parseJSON = withObject "Limit" $ \o -> do
    let keys = KeyMap.keys o
    case keys of
      ["ExplicitLimit"] -> ExplicitLimit <$> o .: "ExplicitLimit"
      ["MaxLimit"] -> pure MaxLimit
      _ -> fail "Invalid Limit"

-- JSON instances for history types
instance ToJSON ExecutionStatus where
  toJSON Success = "Success"
  toJSON Failed = "Failed"
  toJSON DryRun = "DryRun"

instance FromJSON ExecutionStatus where
  parseJSON = \case
    "Success" -> pure Success
    "Failed" -> pure Failed
    "DryRun" -> pure DryRun
    _ -> fail "Invalid ExecutionStatus"

instance ToJSON HistoryEntry where
  toJSON entry = object
    [ "historyId" .= _historyId entry
    , "timestamp" .= _historyTimestamp entry
    , "query" .= _historyQuery entry
    , "logGroups" .= _historyLogGroups entry
    , "timeRange" .= _historyTimeRange entry
    , "limit" .= _historyLimit entry
    , "queryLibrary" .= _historyQueryLibrary entry
    , "executionTime" .= _historyExecutionTime entry
    , "status" .= _historyStatus entry
    ]

instance FromJSON HistoryEntry where
  parseJSON = withObject "HistoryEntry" $ \o -> HistoryEntry
    <$> o .: "historyId"
    <*> o .: "timestamp"
    <*> o .: "query"
    <*> o .: "logGroups"
    <*> o .: "timeRange"
    <*> o .: "limit"
    <*> o .: "queryLibrary"
    <*> o .: "executionTime" 
    <*> o .: "status"

-- Generate lenses for all data types
makeLenses ''RunArgs
makeLenses ''LibraryArgs
makeLenses ''QueryShowArgs
makeLenses ''HistoryEntry
makeLenses ''HistoryArgs
makeLenses ''AppArgs