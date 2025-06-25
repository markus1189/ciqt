# CloudWatch Insights Query Tool

<p align="center">
  <img src="assets/logo.png" alt="CloudWatch Insights Query Tool Logo" width="200">
</p>

A command-line utility for executing and retrieving CloudWatch Insights queries with flexible log group selection and time range options.

**Built with Haskell** for reliability and performance, featuring a sophisticated functional architecture with comprehensive AWS integration, dual query library system, and robust resource management.

## Installation

### Using Nix (Recommended)

```bash
# run without installing
nix run github:markus1189/ciqt -- --help

# For development
nix develop github:markus1189/ciqt
```

### Building from Source

```bash
git clone https://github.com/markus1189/ciqt.git
cd ciqt
nix build
./result/bin/ciqt --help
```

**Note**: The project migrated from hpack (package.yaml) to Cabal for better Nix integration and modular architecture support.

## Features

- Execute CloudWatch Insights queries directly from the command line
- Sub-command structure for organized functionality (`run`, `library`, `query`, `history`)
- **Comprehensive query history tracking:**
  - Automatic recording of all query executions with full parameters
  - Individual JSON files per execution with SHA256 hash IDs
  - Track execution time, status (Success/Failed/DryRun), and metadata
  - List, show, rerun, and clear history entries
  - Partial hash matching for easy reference
  - Configurable history directory
- **Dual query library system:**
  - Local file-based query library for personal queries
  - **AWS CloudWatch Logs Insights saved queries integration**
  - Sync and manage queries between local and AWS
- Multiple ways to specify log groups:
  - Comma-separated list of log group names
  - Pattern matching (substring search)
  - Prefix matching
  - Glob pattern matching
  - Regular expression matching
- **Multiple query sources:**
  - Direct query strings
  - Local files
  - Local library queries
  - **AWS saved queries by ID**
- Flexible time range specification:
  - Absolute time ranges with start and optional end times
  - Relative time ranges (e.g., "query logs from the last 3 hours")
- Control over result limits
- Dry-run mode to preview query parameters without execution
- JSON output for easy integration with other tools
- **Robust architecture**: Functional programming with comprehensive error handling and automatic resource cleanup
- **High performance**: Efficient streaming, memory-conscious design, and optimized AWS API usage

## Usage

```
ciqt [COMMAND] [OPTIONS]
```

### Commands

#### `ciqt run` - Execute CloudWatch Insights Queries

Execute queries against CloudWatch Logs. This is the default command when no sub-command is specified.

```
ciqt run [OPTIONS]
ciqt [OPTIONS]  # Equivalent (backward compatibility)
```

**Options:**
- `--query TEXT` - Specify the CloudWatch Insights query directly
- `--query-file FILE` - Path to a file containing the query
- `--query-name NAME` - Use a saved query from your local library
- `--query-aws-id QUERY_ID` - **Execute an AWS saved query by its definition ID**
- `--start TIME` - Start time in ISO8601 format
- `--end TIME` - End time in ISO8601 format (defaults to current time if omitted)
- `--since DURATION` - Relative time range (ISO8601 duration format)
- `--log-groups LOG_GROUPS` - Comma-separated list of log group names
- `--log-group-pattern PATTERN` - Match log groups by case-sensitive substring
- `--log-group-prefix PREFIX` - Match log groups by case-sensitive prefix
- `--log-group-glob GLOB` - Match log groups using glob pattern
- `--log-group-regex REGEX` - Match log groups using PCRE regex
- `--limit N` - Limit the number of results returned
- `--limit-max` - Use maximum limit for results from AWS (10,000)
- `--dry-run` - Print arguments and query without executing
- `--query-library DIR` - Path to query library directory

#### `ciqt library` - Manage Query Library

Manage your collection of saved queries in both local and AWS libraries.

##### Local Library Commands

##### `ciqt library list`
List all available queries in your local library.

```
ciqt library list [--query-library DIR]
```

##### `ciqt library save <name>`
Save a query to your local library.

```
ciqt library save <name> [--query TEXT | --query-file FILE | --query-name NAME] [--query-library DIR]
```

##### `ciqt library delete <name>`
Delete a query from your local library.

```
ciqt library delete <name> [--query-library DIR]
```

##### `ciqt library show <name>`
Display the content of a saved query from your local library.

```
ciqt library show <name> [--query-library DIR]
```

##### AWS CloudWatch Logs Insights Commands

##### `ciqt library aws-list`
List all saved queries from AWS CloudWatch Logs Insights.

```
ciqt library aws-list
```

##### `ciqt library aws-download <query-id> <local-name>`
Download an AWS saved query to your local library.

```
ciqt library aws-download abc123-def456 aws/lambda/errors
```

##### `ciqt library aws-upload <local-name>`
Upload a local query to AWS CloudWatch Logs Insights.

```
ciqt library aws-upload aws/lambda/errors
```

##### `ciqt library aws-delete <query-id>`
Delete a saved query from AWS CloudWatch Logs Insights.

```
ciqt library aws-delete abc123-def456
```

##### `ciqt library sync`
Show both local and AWS queries for comparison and sync planning.

```
ciqt library sync
```

#### `ciqt query` - Show Query Content

Display query content without execution.

```
ciqt query [--query TEXT | --query-file FILE | --query-name NAME | --query-aws-id QUERY_ID] [--query-library DIR]
```

#### `ciqt history` - Manage Query Execution History

View and manage the history of executed queries. All query executions are automatically recorded with full parameters and results.

##### `ciqt history list`
List all history entries sorted by timestamp (newest first).

```
ciqt history list [--history-dir DIR]
```

##### `ciqt history show <hash>`
Show detailed information about a specific history entry.

```
ciqt history show a1b2c3d4 [--history-dir DIR]
```

Supports partial hash matching - you can use just the first few characters of the hash as long as it's unique.

##### `ciqt history rerun <hash>`
Re-execute a query from history with the same parameters.

```
ciqt history rerun a1b2 [--history-dir DIR]
```

##### `ciqt history clear`
Clear all history entries.

```
ciqt history clear [--history-dir DIR]
```

**Global Options:**
- `--history-dir DIR` - Path to history directory (defaults to `~/.ciqt/history/`)

### Examples

#### Running Queries

Query the last hour of logs from a specific log group:
```bash
ciqt run --log-groups /aws/lambda/my-function --since PT1H --query "fields @timestamp, @message | sort @timestamp desc"
```

Query logs with a specific pattern across multiple log groups:
```bash
ciqt run --log-group-prefix /aws/lambda/ --start 2023-01-01T00:00:00Z --end 2023-01-02T00:00:00Z --query "fields @timestamp, @message | filter @message like 'ERROR'"
```

Load a query from a file and execute in dry-run mode:
```bash
ciqt run --log-group-pattern api-gateway --query-file ./my-query.txt --dry-run
```

Use a saved query from your local library:
```bash
ciqt run --query-name aws/lambda/errors --log-groups /aws/lambda/my-function --since PT24H
```

**Execute an AWS saved query directly:**
```bash
ciqt run --query-aws-id abc123-def456 --log-groups /aws/lambda/my-function --since PT2H
```

#### Working with AWS Saved Queries

List all AWS saved queries:
```bash
ciqt library aws-list
```

Download an AWS query to your local library:
```bash
ciqt library aws-download abc123-def456 lambda/performance-analysis
```

Upload a local query to AWS for team sharing:
```bash
ciqt library aws-upload lambda/error-analysis
```

Compare local and AWS queries:
```bash
ciqt library sync
```

View an AWS query without executing it:
```bash
ciqt query --query-aws-id abc123-def456
```

#### Backward Compatibility

All existing commands work without the `run` sub-command:
```bash
ciqt --log-groups /aws/lambda/my-function --since PT1H --query "fields @timestamp, @message"
```

## Output

The tool outputs:
- Query results as JSON to stdout
- Query metadata and statistics to stderr

## Requirements

- AWS credentials configured (via environment variables, AWS profile, etc.)
- Appropriate IAM permissions for CloudWatch Logs access
- **For AWS saved queries functionality, additional permissions required:**
  - `logs:DescribeQueryDefinitions` - List saved queries
  - `logs:PutQueryDefinition` - Create/update saved queries
  - `logs:DeleteQueryDefinition` - Delete saved queries

#### Managing Library Queries

##### Listing All Queries

View all available queries in your library:

```bash
ciqt library list
```

This displays queries organized by their directory structure.

##### Saving Queries

Save a query to your library:

```bash
ciqt library save errors --query "fields @timestamp, @message | filter @message like 'ERROR'"
```

Save to a subdirectory (directories are created automatically):

```bash
ciqt library save aws/lambda/errors --query "fields @timestamp, @message | filter @message like 'ERROR'"
```

Save from a file:

```bash
ciqt library save my-query --query-file ./my-query.txt
```

##### Viewing Library Queries

Display the content of a saved query:

```bash
ciqt library show errors
```

This works with queries in subdirectories too:

```bash
ciqt library show aws/lambda/errors
```

Alternatively, use the `query` command:

```bash
ciqt query --query-name aws/lambda/errors
```

##### Deleting Queries

Remove a query from your library:

```bash
ciqt library delete errors
```

Delete from subdirectories:

```bash
ciqt library delete aws/lambda/errors
```

### Query Library Structure

By default, the query library is located at `~/.ciqt/queries/`. Each query is stored as a separate file with a `.query` extension. The library supports organizing queries in subdirectories for better management.

Example library structure:
```
~/.ciqt/queries/
├── errors.query
├── aws/
│   ├── lambda/
│   │   ├── errors.query
│   │   └── performance.query
│   └── api-gateway/
│       └── latency.query
└── application/
    └── startup.query
```

### Manual Management

You can also manually manage queries by creating text files with the `.query` extension:

```bash
mkdir -p ~/.ciqt/queries/aws/lambda
echo "fields @timestamp, @message | filter @message like 'ERROR'" > ~/.ciqt/queries/aws/lambda/errors.query
```

## Architecture

### Technical Overview

- **Language**: Haskell with functional programming paradigm
- **Build System**: Nix flakes for reproducible builds and development environments
- **AWS Integration**: Uses amazonka library (≥ 2.0) with comprehensive resource management
- **Architecture**: Modular library design with 7 focused modules and minimal executable wrapper
- **Resource Management**: Automatic cleanup of AWS resources and graceful shutdown handling

### Key Design Principles

- **Modular architecture**: Clean separation of concerns across focused modules
- **Immutable coding style**: Functional approach with minimal side effects
- **Lens-heavy architecture**: Elegant data manipulation with compile-time safety
- **Resource safety**: Automatic cleanup using ResourceT and bracket patterns
- **Type safety**: Strong typing with custom ADTs for domain modeling

## Performance Considerations

### Large-Scale Usage

- **Log Group Discovery**: `--log-group-glob` and `--log-group-regex` retrieve all log groups first, which may be slow in accounts with many log groups (1000+)
- **Query Limits**: AWS CloudWatch Insights has a maximum result limit of 10,000 rows per query
- **Concurrent Queries**: Tool executes one query at a time with proper resource management
- **Memory Usage**: Results are streamed to reduce memory footprint for large result sets

### Optimization Tips

- Use specific log group names (`--log-groups`) when possible for fastest performance
- Use `--log-group-prefix` for efficient filtering without full log group enumeration
- Consider `--limit` for exploratory queries to reduce execution time
- Use `--dry-run` to validate query parameters without execution

## Security

### IAM Permissions

**Basic CloudWatch Logs access:**
```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "logs:StartQuery",
        "logs:GetQueryResults",
        "logs:StopQuery",
        "logs:DescribeLogGroups"
      ],
      "Resource": "*"
    }
  ]
}
```

**For AWS saved queries functionality:**
```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "logs:DescribeQueryDefinitions",
        "logs:PutQueryDefinition",
        "logs:DeleteQueryDefinition"
      ],
      "Resource": "*"
    }
  ]
}
```

### Best Practices

- Use AWS IAM roles instead of long-term access keys when possible
- Scope permissions to specific log groups using resource ARNs
- Monitor CloudWatch Logs usage and costs, especially with broad log group patterns
- Use `--dry-run` to preview queries before execution in production environments

## Development

### Development Environment

```bash
# Enter development shell with all dependencies
nix develop

# Start GHCi REPL with project loaded
nix develop --command ghci

# Format code
nix develop --command ormolu --mode inplace src/Main.hs

# Build and test
nix build
nix run . -- --help
```

### Testing

The project includes a comprehensive test suite built with Tasty framework:

```bash
# Run all tests
nix develop --command cabal test

# Run tests with verbose output
nix develop --command cabal test --test-show-details=streaming

# Run specific test pattern
nix develop --command cabal test --test-options="-p expandTilde"
```

#### Test Coverage

**Unit Tests (2 tests):**
- `expandTilde utility` - Tests path expansion functionality
- `parseNestedJson utility` - Tests JSON parsing with nested structures

#### Test Framework Features

- **Fast Execution**: Tests complete in ~0.5 seconds using `cabal run`
- **Tasty Integration**: Professional test framework with parallel execution
- **CI Ready**: Tests can be easily integrated into continuous integration

#### Updating Golden Files

When something intentionally changes, update golden files:

```bash
# Accept new golden file outputs
nix develop --command cabal test --test-options="--accept"
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes following the existing functional programming style
4. Use `ormolu` for code formatting
5. Test with various AWS environments
6. Submit a pull request

## Troubleshooting

### Common Issues

**Authentication errors:**
```bash
# Verify AWS credentials
aws sts get-caller-identity

# Check AWS profile
export AWS_PROFILE=your-profile
ciqt --help
```

**Query library issues:**
```bash
# Ensure library directory exists
mkdir -p ~/.ciqt/queries

# Check permissions
ls -la ~/.ciqt/
```

**Build issues:**
```bash
# Clean build
nix build --rebuild

# Check Nix installation
nix --version
```

**Performance issues:**
- Use specific log group names instead of patterns when possible
- Consider reducing query time ranges for initial testing
- Use `--limit` to cap result sizes during development

## Notes

- When using `--log-group-glob` or `--log-group-regex`, the tool retrieves all log groups first, which may be slow in accounts with many log groups
- Queries can be stopped with Ctrl+C, and the tool will attempt to stop the running query on AWS
- The tool uses a 30-minute timeout for query execution with 2-second polling intervals
- Results are streamed to stdout as JSON, with progress information sent to stderr
