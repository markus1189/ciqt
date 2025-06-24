# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `ciqt` (CloudWatch Insights Query Tool) - a sophisticated Haskell command-line utility for executing and retrieving CloudWatch Insights queries with flexible log group selection and time range options.

### Technical Architecture
- **Modular library design**: Clean separation into 7 focused modules with minimal executable wrapper
- **Module structure**:
  - `Ciqt.Types`: Core data types and domain models (110 lines)
  - `Ciqt.CLI`: Command-line interface and argument parsing (260 lines)
  - `Ciqt.AWS`: AWS SDK integration and CloudWatch operations (250 lines)
  - `Ciqt.Query`: Query execution and result processing (130 lines)
  - `Ciqt.Library`: Local query library management (70 lines)
  - `Ciqt.Utils`: Pure utility functions and helpers (120 lines)
  - `Ciqt`: Main orchestration and command dispatching (180 lines)
  - `Main`: Minimal 3-line executable wrapper
- **Functional paradigm**: Immutable coding style with lens-heavy architecture
- **Type-safe design**: Comprehensive ADT modeling of domain concepts
- **Resource management**: Advanced ResourceT and bracket patterns for AWS cleanup
- **Monadic architecture**: Strategic use of monad transformers (ResourceT, IO, FailT)

## Development Setup

### Build System

This project uses Nix for build management:
- `nix build` - Build the project (creates `result` symlink to built executable)
- `nix develop` - Enter development shell with all dependencies
- `nix develop --command ghci` - Start GHCi with all dependencies loaded
- `nix run . -- <args>` - Run the built binary with arguments
- `nix shell` - Add ciqt to PATH temporarily

The project uses Cabal (`.cabal` file) for package configuration, having migrated from hpack (`package.yaml`) for better Nix integration and modular architecture support.

### Code Formatting

- **Formatter**: `ormolu` is used for code formatting
- Available in development shell: `nix develop --command ormolu`
- Formatting is automatically applied via development tooling

### Testing

The project uses **Tasty** framework with comprehensive test coverage:

**Framework**: Tasty with tasty-hunit (unit tests) and tasty-golden (CLI regression tests)

**Dependencies**:
- `tasty` - Main test framework
- `tasty-hunit` - Unit testing support
- `tasty-golden` - Golden file testing
- `process`, `bytestring` - Command execution and output handling

**Test Structure**:
- Test suite configuration in `ciqt.cabal` as `test-suite ciqt-test`
- Main test runner at `test/Spec.hs`
- Golden files stored in `test/golden/` directory
- All tests integrated with Nix development environment

**Commands**:
```bash
# Run all tests (fast: ~0.5 seconds)
nix develop --command cabal test

# Run with verbose output
nix develop --command cabal test --test-show-details=streaming

# Accept the new golden files
nix develop --command cabal test --test-options="--accept"

# Run specific test
nix develop --command cabal test --test-options="-p '/main-help-output/'"
```

## Architecture Overview

### Design Principles
- **Modular architecture**: Clean separation of concerns across focused modules
- **Library + executable pattern**: Core functionality in library, minimal executable wrapper
- **Immutable coding style**: Functional approach with minimal side effects
- **Lens-heavy**: Extensive use of lens operators for data manipulation
- **Resource management**: Proper cleanup of AWS queries and resources

### Command Structure
- **Subcommand architecture**: `run`, `library`, `query` commands
- **Backward compatibility**: Defaults to `run` command for legacy usage
- **Flexible argument parsing**: Uses `optparse-applicative` with rich help

## Core Components

### Key Data Types
- `LogGroupsArg`: Union type for different log group selection methods (comma-separated, pattern, prefix, glob, regex)
- `TimeRange`: Absolute or relative time ranges for queries
- `QueryArg`: Query source (file, string, library, or AWS saved query)
- `AppArgs`: Main application configuration with global options

### Core Functions and Architecture Patterns

**Query Resolution Pipeline:**
```haskell
calculateLogGroups :: AWS.Env -> LogGroupsArg -> IO (Maybe (NonEmpty Text))
-- Resolves 5 different log group selection methods:
-- CommaLogGroups, LogNamePattern, LogNamePrefix, LogNameGlob, LogNameRegex
```

**Query Execution with Resource Management:**
```haskell
executeQuery :: AWS.Env -> Logs.StartQuery -> ResourceT IO (Maybe Logs.GetQueryResultsResponse)
-- Features:
-- - Automatic AWS resource cleanup using bracket pattern
-- - 30-minute timeout with 2-second polling intervals
-- - Graceful query cancellation on SIGINT
-- - Real-time progress statistics
```

**Query Source Abstraction:**
```haskell
calculateQuery :: QueryArg -> Maybe FilePath -> IO Text
-- Unified interface for 4 query sources:
-- QueryFile, QueryString, QueryLibrary, QueryAWS
```

**Main Program Architecture:**
```haskell
mainProgram :: IO ()
-- Top-level orchestration with:
-- - Comprehensive exception handling
-- - AWS environment discovery
-- - Command routing and execution
-- - Proper exit code management
```

## AWS Integration

### Dependencies
The project uses 29 carefully selected Haskell packages:

**Core AWS Integration:**
- `amazonka >= 2.0` - Modern AWS SDK with comprehensive CloudWatch Logs support
- `amazonka-core >= 2.0` - Core AWS functionality with automatic credential discovery
- `amazonka-cloudwatch-logs >= 2.0` - CloudWatch Logs API bindings

**Functional Programming Infrastructure:**
- `lens` - Lens operators for elegant data manipulation (`^.`, `.~`, `?~`, `&`)
- `lens-aeson` - JSON manipulation through lens interface
- `optparse-applicative` - Sophisticated command-line argument parsing
- `resourcet` - Resource management and automatic cleanup
- `retry` - Exponential backoff and retry logic for AWS operations

**Domain-Specific Libraries:**
- `Glob` - Shell-style pattern matching for log group selection
- `regex-pcre` - Full PCRE regex support for advanced log group filtering
- `fmt` + `formatting` - Human-readable output formatting with number localization
- `fast-logger` - High-performance structured logging
- `neat-interpolation` - Clean multi-line string literals

**Utility Libraries:**
- `aeson` - JSON parsing and generation
- `time` - Comprehensive time handling with ISO8601 support
- `either` - Enhanced Either operations
- `exceptions` - Exception handling with MonadCatch
- `split` - List manipulation utilities

### Authentication and AWS Integration

**Environment Discovery:**
```haskell
discoverAwsEnv :: (MonadIO m, MonadCatch m) => FastLogger -> m AWS.Env
-- Automatic credential discovery with integrated logging
-- Supports: env vars, profiles, IAM roles, instance metadata
```

**AWS Integration Patterns:**
- **Credential chain**: Standard AWS credential discovery with fallbacks
- **Region handling**: Automatic region detection from environment
- **Logging integration**: Custom FastLogger bridge for AWS SDK logging
- **Error handling**: Comprehensive AWS exception catching and user-friendly error messages
- **Pagination**: Proper handling of AWS API pagination for large result sets

**Required IAM Permissions:**
```json
{
  "logs:StartQuery", "logs:GetQueryResults", "logs:StopQuery",
  "logs:DescribeLogGroups", "logs:DescribeQueryDefinitions",
  "logs:PutQueryDefinition", "logs:DeleteQueryDefinition"
}
```

### Query Library System

The tool supports a comprehensive query library system:

#### Local Queries
- Queries stored as `.query` files in `~/.ciqt/queries/` (or custom directory)
- Directory tree organization supported (e.g., `aws/lambda/errors.query`)
- Query management via CLI: `ciqt library list|save|delete|show`
- Queries referenced by path: `--query-name aws/lambda/errors`

#### AWS Saved Queries
- Integration with AWS CloudWatch Logs Insights saved queries
- Commands: `ciqt library aws-list|aws-download|aws-upload|aws-delete`
- Bidirectional sync capabilities: `ciqt library sync`
- Query execution directly from AWS: `--query-aws-id <query-id>`

#### Library Operations
- Library operations work independently of time/log group parameters
- Supports nested directory structures for organization
- Automatic `.query` extension handling

## Development Workflow

### Development Environment Setup

1. **Enter Development Shell**:
   ```bash
   nix develop                         # Full development environment
   nix develop --command ghci          # Direct GHCi with all dependencies
   ```

2. **Development Tools Available**:
   - `haskell-language-server` - LSP support for editors
   - `ghcid` - Fast feedback during development
   - `cabal-install` - Package management
   - `ormolu` - Code formatting
   - `withHoogle` - Local documentation server

### Code Development Patterns

**Lens-Heavy Development:**
```haskell
-- Reading with lens operators
runArgs ^. runArgsQuery              -- Extract query
runArgs ^. runArgsTimeRange . _Just  -- Extract optional time range

-- Writing with lens operators
env & AWS.logger .~ customLogger     -- Set logger
query & startQuery_limit ?~ n        -- Maybe set limit
args & runArgsLogGroups .~ Just lgs  -- Set log groups
```

**Resource Management Patterns:**
```haskell
-- Standard bracket pattern for AWS resources
bracket
  (AWS.send env startQuery)          -- Acquire
  (\resp -> AWS.send env (stopQuery (resp ^. startQueryResponse_queryId))) -- Release
  (\resp -> pollForResults env resp)  -- Use
```

**Error Handling Patterns:**
```haskell
-- Top-level exception handling
result <- try @_ @SomeException $ action
case result of
  Left err -> do
    TIO.hPutStrLn stderr ("Error: " <> tshow err)
    exitFailure
  Right value -> pure value
```

### Testing and Validation

1. **Automated Test Suite**:
   ```bash
   # Run all tests (fast: ~0.5 seconds)
   nix develop --command cabal test

   # Run tests with verbose output
   nix develop --command cabal test --test-show-details=streaming

   # Update golden files
   nix develop --command cabal test --test-options="--accept"

   # Test specific components
   nix develop --command cabal test --test-options="-p expandTilde"
   ```

2. **Manual Integration Testing** (requires AWS credentials):
   ```bash
   # Test query execution
   nix run . -- --query 'fields @timestamp | limit 5' --log-groups '/aws/lambda/test' --since PT1H --dry-run

   # Test library operations
   nix run . -- library save test --query 'fields @timestamp'
   nix run . -- library list
   nix run . -- library show test
   nix run . -- library delete test
   ```

3. **Performance Testing**:
   ```bash
   # Test with large result sets
   nix run . -- --query 'fields @timestamp' --limit-max --log-groups '/aws/lambda/high-volume'

   # Test log group discovery performance
   nix run . -- --log-group-pattern 'api' --dry-run  # Should be fast
   nix run . -- --log-group-regex '.*' --dry-run     # May be slow
   ```

### Common Modification Patterns

**Adding New Command-Line Options:**
1. **Update Data Types** in `src/Ciqt/Types.hs`:
   ```haskell
   data RunArgs = RunArgs
     { _runArgsNewOption :: Maybe NewType  -- Add new field
     , ... existing fields
     }
   ```

2. **Add Lens Generation** in `src/Ciqt/Types.hs`:
   ```haskell
   makeLenses ''RunArgs  -- Automatically generates lenses
   ```

3. **Update Argument Parser** in `src/Ciqt/CLI.hs`:
   ```haskell
   runArgsParser = RunArgs
     <$> existingParsers
     <*> optional newOptionParser
   ```

4. **Use in Main Logic** in `src/Ciqt.hs`:
   ```haskell
   let newValue = args ^. runArgs . runArgsNewOption
   ```

**Adding New Log Group Selection Methods:**
1. **Extend LogGroupsArg ADT** in `src/Ciqt/Types.hs`:
   ```haskell
   data LogGroupsArg =
     | ExistingConstructors
     | NewLogGroupMethod NewType
   ```

2. **Update Show instance** in `src/Ciqt/Types.hs`
3. **Extend calculateLogGroups function** in `src/Ciqt/AWS.hs` with new pattern matching
4. **Add corresponding CLI parser** in `src/Ciqt/CLI.hs`

**Adding New Query Sources:**
1. **Extend QueryArg ADT** in `src/Ciqt/Types.hs`:
   ```haskell
   data QueryArg =
     | ExistingConstructors
     | NewQuerySource NewType
   ```

2. **Update calculateQuery function** in `src/Ciqt/Query.hs` with new pattern matching
3. **Add error handling for new source type**

**Resource Management Guidelines:**
- Always use `bracket` for AWS resource acquisition/cleanup
- Implement proper exception handling with `try`
- Use `ResourceT` for complex resource management scenarios
- Follow the existing pattern of cleanup functions that stop running queries

**Lens Usage Best Practices:**
- Use `^.` for reading values
- Use `.~` for setting values
- Use `?~` for setting Maybe values
- Use `&` for chaining operations
- Use `%~` for transforming values

**Error Handling Standards:**
- Use `ExitFailure` with specific exit codes
- Send user-friendly messages to stderr
- Log technical details with FastLogger
- Always handle AWS exceptions gracefully

**Performance Considerations:**
- Use `NonEmpty` for guaranteed non-empty lists
- Stream results to stdout to avoid memory accumulation
- Implement proper pagination for AWS API calls
- Use lazy evaluation where appropriate (Text, ByteString)

### Troubleshooting and Common Issues

**Build and Development Issues:**
- **Dependency conflicts**: Use `nix flake update` to update flake.lock if builds fail
- **GHC version mismatch**: The flake pins to a specific nixpkgs revision for consistency
- **Missing dependencies**: All 29 required packages are managed by Nix
- **ormolu formatting**: Run `nix develop --command ormolu --mode inplace src/**/*.hs` to format all modules

**AWS Integration Issues:**
- **Authentication failures**: Check credential chain with `aws sts get-caller-identity`
- **Region issues**: Set `AWS_DEFAULT_REGION` or ensure credentials include region
- **Permission errors**: Verify IAM permissions for CloudWatch Logs operations
- **Query timeout**: 30-minute hard limit - check for resource-intensive queries
- **Rate limiting**: AWS may throttle API calls - implement backoff if needed

**Performance Issues:**
- **Slow log group discovery**: Use specific log group names or prefixes instead of patterns/regex
- **Large result sets**: Consider using `--limit` or more specific queries
- **Memory usage**: Results are streamed but very large queries may still consume memory

**Query Library Issues:**
- **Directory permissions**: Ensure `~/.ciqt/queries/` is writable
- **File extension**: Query files must have `.query` extension
- **Path resolution**: Check tilde expansion works in your environment
- **AWS sync conflicts**: Local and AWS query names must be unique

**Runtime Debugging:**
```bash
# Enable AWS debug logging
export AWS_LOG_LEVEL=debug

# Test with dry-run first
nix run . -- --dry-run --query 'test' --log-groups 'test'

# Verify AWS connectivity
aws logs describe-log-groups --limit 1

# Check query library
ls -la ~/.ciqt/queries/
```

**Development Debugging:**
```bash
# Fast feedback during development
nix develop --command ghcid

# Load in GHCi for interactive testing
nix develop --command ghci  # Will load all modules

# Check code formatting
nix develop --command ormolu --mode check src/**/*.hs
```

### Architecture-Specific Guidelines

**Modular Architecture Benefits:**
- **Pros**: Improved maintainability, faster compilation, better testability, easier debugging
- **Clear boundaries**: Each module has focused responsibilities and clean interfaces
- **Separation of concerns**: Pure functions separated from IO operations and AWS integration
- **Testability**: Individual modules can be tested in isolation
- **Development workflow**: Faster incremental compilation and clearer error messages

**Lens Architecture Benefits:**
- **Elegant composition**: Chain operations with `&` operator
- **Type safety**: Compile-time verification of field access
- **Readability**: Clear intent with operators like `^.` and `.~`
- **Maintainability**: Easy refactoring when data structures change

**Functional Programming Patterns:**
- **Immutability**: All data structures are immutable by default
- **Pure functions**: Separate pure logic from IO operations
- **Monad composition**: Use monadic chains for sequential operations
- **Error handling**: Prefer Either/Maybe over exceptions for pure code

**Resource Management Philosophy:**
- **Acquire-Release**: Always pair resource acquisition with cleanup
- **Exception safety**: Use bracket patterns for guaranteed cleanup
- **Timeout handling**: All AWS operations have bounded execution time
- **Graceful shutdown**: Handle interruption signals properly

### Extension Points for New Features

**Adding New Output Formats:**
1. Extend result processing pipeline after query execution
2. Add command-line options for format selection
3. Maintain backward compatibility with JSON default

**Adding New Time Range Formats:**
1. Extend `TimeRange` ADT with new constructor
2. Update time parsing logic
3. Add corresponding CLI parser

**Adding New Authentication Methods:**
1. Extend AWS environment discovery function
2. Add new credential providers to amazonka configuration
3. Maintain fallback to standard credential chain

**Adding Concurrent Query Execution:**
1. Would require significant architectural changes
2. Consider resource limits and AWS API throttling
3. Implement proper resource cleanup for multiple queries

### Testing Strategy

**Current Test Suite** (5 tests total, ~0.5s execution):

**Unit Testing** (2 tests):
- `expandTilde utility` - Tests path expansion from `Ciqt.Utils:44-53`
- `parseNestedJson utility` - Tests JSON parsing from `Ciqt.Utils:79-83`

**Test Implementation**:
```haskell
-- test/Spec.hs uses cabal run for fast execution
getCiqtHelpOutput :: [String] -> IO LBS.ByteString
getCiqtHelpOutput args = do
  (_, stdout, stderr) <- readProcessWithExitCode "cabal" (["run", "ciqt", "--"] ++ args) ""
  let output = if null stderr then stdout else stderr
  pure $ LBS8.pack output
```

**Benefits**:
- **CLI Regression Protection**: Automatically catches help text changes
- **Fast Feedback**: Sub-second execution vs previous 60+ second nix runs
- **Real Binary Testing**: Tests actual compiled behavior vs synthetic tests
- **CI Integration**: Ready for automated testing pipelines

**Future Testing Opportunities**:
- Property-based testing with QuickCheck for pure functions
- Mock AWS operations for testing business logic
- Integration tests with AWS services (requires credentials)
- Performance benchmarking for log group discovery patterns
