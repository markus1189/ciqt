# History Feature Implementation Plan

## Overview
Add a history feature that records every executed query with its arguments to individual JSON files in a configurable directory. History entries can be referenced using short SHA256 hashes as filenames.

## High-Level Architecture

### Data Flow
1. **Query Execution**: Before executing any query, generate a history ID and save entry
2. **History Recording**: Save query parameters and metadata to `{hash}.json` file
3. **Result Recording**: Update history entry with execution results and timing
4. **History Management**: Provide commands to list, show, rerun, and clear history

### Core Components
- **HistoryEntry**: Data type representing a single query execution
- **History Module**: Core logic for saving/loading/managing history files
- **CLI Integration**: Commands and options for history management
- **Main Integration**: Hook history recording into query execution flow

## Detailed Implementation Steps

### Phase 1: Data Types and Core Infrastructure

#### Step 1.1: Add Data Types to `src/Ciqt/Types.hs`
```haskell
-- New data types to add:
data ExecutionStatus = Success | Failed | DryRun
  deriving (Eq, Show, Generic)

data HistoryEntry = HistoryEntry
  { _historyId :: Text                    -- 8-char SHA256 hash
  , _historyTimestamp :: UTCTime          -- When query was executed
  , _historyQuery :: Text                 -- The query text
  , _historyLogGroups :: LogGroupsArg     -- Log groups specification  
  , _historyTimeRange :: TimeRange        -- Time range specification
  , _historyLimit :: Maybe Limit          -- Result limit
  , _historyQueryLibrary :: Maybe FilePath -- Query library path used
  , _historyExecutionTime :: Maybe NominalDiffTime -- How long execution took
  , _historyStatus :: ExecutionStatus     -- Success/Failed/DryRun
  }
  deriving (Eq, Show, Generic)

data HistoryOperation
  = ListHistory           -- List all history entries
  | ShowHistory Text      -- Show details of specific entry by hash
  | RerunHistory Text     -- Re-execute query from history by hash  
  | ClearHistory          -- Clear all history entries
  deriving (Show)

data HistoryArgs = HistoryArgs
  { _historyArgsOperation :: HistoryOperation
  , _historyArgsHistoryDir :: Maybe FilePath  -- Optional custom history directory
  }
  deriving (Show)
```

#### Step 1.2: Update Command Types
```haskell
-- Add to Command ADT:
data Command
  = RunCommand RunArgs
  | LibraryCommand LibraryArgs  
  | QueryShowCommand QueryShowArgs
  | HistoryCommand HistoryArgs    -- NEW
  deriving (Show)

-- Add to AppArgs:
data AppArgs = AppArgs
  { _appArgsCommand :: Command
  , _appArgsGlobalQueryLibrary :: Maybe FilePath
  , _appArgsGlobalHistoryDir :: Maybe FilePath  -- NEW
  }
  deriving (Show)
```

#### Step 1.3: Add Lens Generation
```haskell
-- Add to makeLenses calls:
makeLenses ''HistoryEntry
makeLenses ''HistoryArgs  
makeLenses ''AppArgs  -- Update existing
```

#### Step 1.4: Add JSON Instances
```haskell
-- Derive Generic and add ToJSON/FromJSON instances
instance ToJSON ExecutionStatus
instance FromJSON ExecutionStatus
instance ToJSON HistoryEntry  
instance FromJSON HistoryEntry
```

### Phase 2: Core History Module

#### Step 2.1: Create `src/Ciqt/History.hs`
```haskell  
module Ciqt.History
  ( -- * History Management
    recordHistoryEntry,
    updateHistoryEntry,
    loadHistoryEntry,
    listAllHistoryEntries,
    findHistoryEntry,
    clearHistory,
    
    -- * Utilities
    generateHistoryId,
    getHistoryDirPath,
  )
where
```

#### Step 2.2: Implement Core Functions
- `generateHistoryId`: Create 8-char SHA256 hash from query parameters
- `getHistoryDirPath`: Resolve history directory (default `~/.ciqt/history/`)
- `recordHistoryEntry`: Save new history entry to `{hash}.json`
- `updateHistoryEntry`: Update existing entry with execution results
- `loadHistoryEntry`: Load specific entry by hash
- `findHistoryEntry`: Find entry by partial hash using glob matching
- `listAllHistoryEntries`: Load all entries and sort by timestamp
- `clearHistory`: Remove all `.json` files from history directory

### Phase 3: CLI Integration

#### Step 3.1: Update `src/Ciqt/CLI.hs`
- Add `--history-dir` global option to `appArgsParser`
- Add `history` subcommand to `commandParser`
- Implement `historyArgsParser` with subcommands:
  - `list`: List all history entries
  - `show <hash>`: Show specific entry details
  - `rerun <hash>`: Re-execute query from history
  - `clear`: Clear all history
- Add `--from-history <hash>` option to `runArgsParser`

### Phase 4: Main Program Integration

#### Step 4.1: Update `src/Ciqt.hs`
- Add `handleHistoryCommand` function
- Modify `handleRunCommand` to:
  - Generate history ID before execution
  - Record initial history entry
  - Update entry with execution results
  - Handle `--from-history` option for re-execution
- Add history recording to main program flow

### Phase 5: Testing

#### Step 5.1: Add Dependencies to `ciqt.cabal`
```cabal
build-depends:
  -- existing dependencies...
  , cryptohash
  , QuickCheck
```

#### Step 5.2: Unit Tests in `test/Spec.hs`
- Test hash generation consistency
- Test history entry save/load roundtrip
- Test partial hash matching
- Test directory operations

#### Step 5.3: QuickCheck Property Tests
- JSON serialization roundtrip properties for `HistoryEntry`
- Hash uniqueness properties
- File system operation properties

## Example Usage After Implementation

```bash
# Execute query (automatically recorded in history)
ciqt run --query "fields @timestamp | limit 10" --log-groups "/aws/lambda/func" --since PT1H

# List history
ciqt history list

# Show specific entry
ciqt history show a1b2c3d4

# Re-run from history (partial hash)
ciqt history rerun a1b2

# Re-run with different parameters  
ciqt run --from-history a1b2 --limit 100

# Clear all history
ciqt history clear

# Use custom history directory
ciqt --history-dir ~/my-queries run --query "..." --log-groups "..."
```

## File Structure After Implementation

```
~/.ciqt/
├── queries/          # Existing query library
│   └── *.query
└── history/          # New history directory  
    ├── a1b2c3d4.json # Individual history entries
    ├── e5f6g7h8.json
    └── 9i0j1k2l.json
```

## Implementation Notes

### Dependencies
- `cryptohash`: For SHA256 hash generation
- `QuickCheck`: For property-based testing
- Existing: `aeson`, `time`, `directory`, `filepath`, `Glob`

### Design Decisions
- **One file per entry**: Avoids concurrency issues, enables atomic operations
- **JSON format**: Human-readable, tooling-friendly
- **8-char hashes**: Balance between collision resistance and usability
- **Partial hash matching**: User-friendly, similar to git
- **Configurable directory**: Flexible deployment options

### Error Handling
- Handle hash collisions gracefully
- Validate partial hash uniqueness
- Proper error messages for missing/corrupted history files
- Fallback behavior when history directory doesn't exist