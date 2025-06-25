# CloudWatch Insights Query Tool (CIQT)

<p align="center">
  <img src="assets/logo.png" alt="CloudWatch Insights Query Tool Logo" width="200">
</p>

<p align="center">
  <strong>A command-line utility for executing and managing CloudWatch Insights queries</strong>
</p>

<p align="center">
  <a href="#installation">Installation</a> •
  <a href="#features">Features</a> •
  <a href="#usage">Usage</a> •
  <a href="#examples">Examples</a> •
  <a href="#architecture">Architecture</a> •
  <a href="#development">Development</a>
</p>

---

## Overview

CIQT provides sophisticated tooling for CloudWatch Logs analysis with comprehensive query management, execution history tracking, and seamless AWS integration. Built with Haskell for reliability and performance, it features a functional architecture with comprehensive resource management and type safety.

### Key Capabilities

- **Advanced Query Execution**: Multiple query sources with flexible log group selection
- **Comprehensive History Tracking**: Automatic recording with SHA256-based indexing
- **Dual Library System**: Local and AWS CloudWatch Logs Insights integration
- **Tool Integration**: JSON output, dry-run validation, and robust error handling
- **High Performance**: Streaming results, memory-conscious design, optimized AWS API usage

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

### Core Functionality

- **Command-Line Query Execution**: Direct CloudWatch Insights query execution with comprehensive parameter control
- **Modular Command Structure**: Organized functionality through `run`, `library`, `query`, and `history` subcommands
- **Tool Integration**: JSON output format, dry-run validation, and robust error handling

### Query History Management

- **Automatic Execution Tracking**: Complete parameter and result recording for all query executions
- **SHA256-Based Indexing**: Individual JSON files per execution with cryptographic hash identification
- **Comprehensive Metadata**: Execution time, status tracking (Success/Failed/DryRun), and full parameter history
- **Advanced History Operations**: List, show, rerun, and clear capabilities with partial hash matching
- **Configurable Storage**: Customizable history directory with organized file structure

### Query Library System

- **Dual Library Architecture**: 
  - Local file-based query management for personal collections
  - Native AWS CloudWatch Logs Insights saved queries integration
- **Synchronization Capabilities**: Bidirectional sync between local and AWS query libraries
- **Hierarchical Organization**: Directory-based query organization with automatic management

### Log Group Selection

- **Multiple Selection Methods**:
  - Direct specification via comma-separated log group names
  - Pattern-based matching with substring search
  - Prefix-based filtering for namespace organization
  - Advanced glob pattern matching for complex selections
  - Full PCRE regular expression support for sophisticated filtering

### Query Source Flexibility

- **Multiple Input Methods**:
  - Direct command-line query strings
  - Local file-based query loading
  - Local library query references
  - Direct AWS saved query execution by ID

### Time Range Management

- **Flexible Time Specification**:
  - Absolute time ranges with ISO8601 format support
  - Relative time ranges using ISO8601 duration format
  - Intelligent defaults with optional end time specification

### Performance & Reliability

- **High-Performance Architecture**: Efficient streaming, memory-conscious design, optimized AWS API usage
- **Robust Resource Management**: Automatic cleanup using ResourceT and bracket patterns
- **Type-Safe Design**: Comprehensive ADT modeling with compile-time safety guarantees
- **Functional Programming**: Immutable architecture with lens-based data manipulation

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

## Examples

### Basic Query Execution

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

### AWS Saved Queries Integration

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

### History Management Examples

View recent query executions:
```bash
ciqt history list
```

Show detailed information for a specific execution:
```bash
ciqt history show a1b2c3d4
```

Re-execute a previous query:
```bash
ciqt history rerun a1b2
```

Clear all execution history:
```bash
ciqt history clear
```

### Backward Compatibility

All existing commands work without the `run` sub-command:
```bash
ciqt --log-groups /aws/lambda/my-function --since PT1H --query "fields @timestamp, @message"
```

## Output Format

CIQT provides structured output designed for both human consumption and programmatic integration:

- **Query Results**: JSON-formatted log events streamed to stdout
- **Metadata & Statistics**: Query execution information and AWS statistics to stderr
- **Structured Logging**: Comprehensive execution tracking with configurable verbosity

## Prerequisites

### AWS Configuration

- **Credentials**: AWS credentials configured via environment variables, AWS profiles, or IAM roles
- **Region**: Appropriate AWS region configuration for target CloudWatch Logs

### IAM Permissions

**Required for basic functionality:**
- `logs:StartQuery` - Execute CloudWatch Insights queries
- `logs:GetQueryResults` - Retrieve query results
- `logs:StopQuery` - Cancel running queries
- `logs:DescribeLogGroups` - Discover and validate log groups

**Additional permissions for AWS saved queries:**
- `logs:DescribeQueryDefinitions` - List saved queries
- `logs:PutQueryDefinition` - Create/update saved queries
- `logs:DeleteQueryDefinition` - Delete saved queries

## Configuration

### Query Library Structure

The local query library uses a hierarchical file structure for organization:

**Default Location**: `~/.ciqt/queries/`

**File Format**: Individual `.query` files with plain text CloudWatch Insights queries

**Example Structure**:
```
~/.ciqt/queries/
├── errors.query                    # Root-level queries
├── aws/
│   ├── lambda/
│   │   ├── errors.query            # Lambda-specific error queries
│   │   └── performance.query       # Performance analysis queries
│   └── api-gateway/
│       └── latency.query           # API Gateway latency queries
└── application/
    ├── startup.query               # Application startup analysis
    └── monitoring.query            # General monitoring queries
```

### History Storage

Execution history is automatically managed with the following structure:

**Default Location**: `~/.ciqt/history/`

**File Format**: Individual JSON files per execution with SHA256-based naming

**Example Structure**:
```
~/.ciqt/history/
├── a1b2c3d4.json                   # Individual execution records
├── e5f6g7h8.json                   # Each file contains complete execution metadata
└── ...                             # Automatic cleanup and organization
```

### Manual Query Management

For advanced users, queries can be managed directly through the filesystem:

```bash
# Create query directory structure
mkdir -p ~/.ciqt/queries/aws/lambda

# Add query directly
echo "fields @timestamp, @message | filter @message like 'ERROR'" > ~/.ciqt/queries/aws/lambda/errors.query

# Verify query integration
ciqt library list
```

## Architecture

### Technical Foundation

**Language & Paradigm**
- **Haskell**: Functional programming with strong type safety and immutability
- **Modular Design**: 7 focused modules with clean separation of concerns
- **Lens Architecture**: Elegant data manipulation with compile-time safety guarantees

**Build & Deployment**
- **Nix Flakes**: Reproducible builds and hermetic development environments
- **Cabal Integration**: Modern package management with dependency resolution
- **Cross-Platform**: Linux, macOS, and NixOS support

**AWS Integration**
- **Amazonka SDK**: Modern AWS SDK (≥ 2.0) with comprehensive CloudWatch Logs support
- **Resource Management**: Automatic cleanup using ResourceT and bracket patterns
- **Credential Chain**: Standard AWS credential discovery with environment integration

### Design Principles

**Reliability**
- **Type Safety**: Comprehensive ADT modeling prevents runtime errors
- **Resource Safety**: Guaranteed cleanup of AWS resources and file handles
- **Exception Handling**: Comprehensive error management with graceful degradation

**Performance**
- **Streaming Architecture**: Memory-efficient result processing for large datasets
- **Optimized API Usage**: Intelligent pagination and request batching
- **Concurrent Design**: Parallel execution where appropriate with proper synchronization

**Maintainability**
- **Immutable Data Structures**: Functional approach minimizes side effects
- **Pure Functions**: Clear separation between IO operations and business logic
- **Comprehensive Testing**: Unit tests, integration tests, and property-based testing

## Performance & Scalability

### Large-Scale Considerations

**Log Group Discovery**
- **Direct Selection**: Fastest performance using specific log group names (`--log-groups`)
- **Prefix Filtering**: Efficient namespace-based filtering (`--log-group-prefix`)
- **Pattern Matching**: Moderate performance for substring matching (`--log-group-pattern`)
- **Advanced Patterns**: `--log-group-glob` and `--log-group-regex` require full enumeration (slower for 1000+ log groups)

**Query Execution**
- **AWS Limits**: Maximum 10,000 rows per query (AWS CloudWatch Insights limitation)
- **Streaming Results**: Memory-efficient processing for large result sets
- **Resource Management**: Single query execution with proper AWS resource cleanup
- **Timeout Handling**: 30-minute execution limit with 2-second polling intervals

### Performance Optimization

**Query Strategy**
- Use specific log group names for maximum performance
- Leverage prefix matching for namespace-based selections
- Apply result limits (`--limit`) for exploratory analysis
- Utilize dry-run mode (`--dry-run`) for parameter validation

**Memory Management**
- Results streamed directly to stdout to minimize memory usage
- Automatic cleanup of temporary resources and AWS connections
- Efficient JSON processing with lazy evaluation where appropriate

## Security & Best Practices

### IAM Configuration

**Minimal Required Permissions**
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

**Extended Permissions for AWS Saved Queries**
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

### Security Best Practices

**Credential Management**
- Use AWS IAM roles instead of long-term access keys
- Leverage AWS profile-based authentication for multi-account access
- Implement credential rotation policies for enhanced security

**Access Control**
- Scope permissions to specific log groups using resource ARNs when possible
- Implement least-privilege access principles
- Regular audit of CloudWatch Logs access patterns

**Operational Security**
- Use `--dry-run` for query validation in production environments
- Monitor CloudWatch Logs usage and associated costs
- Implement query review processes for broad log group patterns

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
- **Tasty Integration**: Modern test framework with parallel execution
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

## Operational Notes

### Query Execution Behavior

- **Interruption Handling**: Queries can be safely interrupted with Ctrl+C, automatically stopping AWS query execution
- **Timeout Management**: 30-minute maximum execution time with 2-second polling intervals
- **Output Streaming**: Results streamed to stdout as JSON with progress information on stderr
- **Resource Cleanup**: Automatic cleanup of AWS resources on normal or abnormal termination

### Performance Considerations

- **Log Group Enumeration**: `--log-group-glob` and `--log-group-regex` require full log group discovery (potentially slow for large AWS accounts)
- **Query Optimization**: Consider log group count and query complexity when designing automated workflows
- **Rate Limiting**: AWS API rate limits may affect operations in high-frequency usage scenarios

---

<p align="center">
  <strong>CIQT - CloudWatch Insights Query Management</strong><br/>
  Built with ❤️ using Haskell and Nix
</p>
