# CloudWatch Insights Query Tool

<p align="center">
  <img src="assets/logo.png" alt="CloudWatch Insights Query Tool Logo" width="200">
</p>

A command-line utility for executing and retrieving CloudWatch Insights queries with flexible log group selection and time range options.

## Features

- Execute CloudWatch Insights queries directly from the command line
- Multiple ways to specify log groups:
  - Comma-separated list of log group names
  - Pattern matching (substring search)
  - Prefix matching
  - Glob pattern matching
  - Regular expression matching
- Flexible time range specification:
  - Absolute time ranges with start and optional end times
  - Relative time ranges (e.g., "query logs from the last 3 hours")
- Control over result limits
- Dry-run mode to preview query parameters without execution
- JSON output for easy integration with other tools

## Usage

```
ciqt [OPTIONS]
```

### Options

#### Query Specification
- `--query TEXT` - Specify the CloudWatch Insights query directly
- `--query-file FILE` - Path to a file containing the query

#### Time Range
- `--start TIME` - Start time in ISO8601 format
- `--end TIME` - End time in ISO8601 format (defaults to current time if omitted)
- `--since DURATION` - Relative time range (ISO8601 duration format)

#### Log Group Selection
- `--log-groups LOG_GROUPS` - Comma-separated list of log group names
- `--log-group-pattern PATTERN` - Match log groups by case-sensitive substring
- `--log-group-prefix PREFIX` - Match log groups by case-sensitive prefix
- `--log-group-glob GLOB` - Match log groups using glob pattern (retrieves all log groups first)
- `--log-group-regex REGEX` - Match log groups using PCRE regex (retrieves all log groups first)

#### Result Limiting
- `--limit N` - Limit the number of results returned
- `--limit-max` - Use maximum limit for results from AWS (10,000)

#### Other Options
- `--dry-run` - Print arguments and query without executing
- `--help` - Show help message

### Examples

Query the last hour of logs from a specific log group:
```
cloudwatch-insights-query --log-groups /aws/lambda/my-function --since PT1H --query "fields @timestamp, @message | sort @timestamp desc"
```

Query logs with a specific pattern across multiple log groups:
```
cloudwatch-insights-query --log-group-prefix /aws/lambda/ --start 2023-01-01T00:00:00Z --end 2023-01-02T00:00:00Z --query "fields @timestamp, @message | filter @message like 'ERROR'"
```

Load a query from a file and execute in dry-run mode:
```
cloudwatch-insights-query --log-group-pattern api-gateway --query-file ./my-query.txt --dry-run
```

## Output

The tool outputs:
- Query results as JSON to stdout
- Query metadata and statistics to stderr

## Requirements

- AWS credentials configured (via environment variables, AWS profile, etc.)
- Appropriate IAM permissions for CloudWatch Logs access

## Query Library

The tool supports maintaining a library of saved queries for easy reuse.

### Library Structure

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

### Using Library Queries

To use a query from your library:

```bash
ciqt --query-name my-query --log-groups /aws/lambda/my-function
```

For queries in subdirectories, use forward slashes:

```bash
ciqt --query-name aws/lambda/errors --log-groups /aws/lambda/my-function
```

You can specify a custom library location:

```bash
ciqt --query-name my-query --query-library /path/to/queries --log-groups /aws/lambda/my-function
```

### Viewing Library Queries

To view the content of a saved query without executing it:

```bash
ciqt --query-name my-query --show-query
```

This works with queries in subdirectories too:

```bash
ciqt --query-name aws/lambda/errors --show-query
```

### Managing Library Queries

#### Listing All Queries

View all available queries in your library:

```bash
ciqt --list-queries
```

This displays queries organized by their directory structure.

#### Saving Queries

Save a query to your library:

```bash
ciqt --query "fields @timestamp, @message | filter @message like 'ERROR'" --save-query errors
```

Save to a subdirectory (directories are created automatically):

```bash
ciqt --query "fields @timestamp, @message | filter @message like 'ERROR'" --save-query aws/lambda/errors
```

#### Deleting Queries

Remove a query from your library:

```bash
ciqt --delete-query errors
```

Delete from subdirectories:

```bash
ciqt --delete-query aws/lambda/errors
```

#### Manual Management

You can also manually manage queries by creating text files with the `.query` extension:

```bash
mkdir -p ~/.ciqt/queries/aws/lambda
echo "fields @timestamp, @message | filter @message like 'ERROR'" > ~/.ciqt/queries/aws/lambda/errors.query
```

## Notes

- When using `--log-group-glob` or `--log-group-regex`, the tool retrieves all log groups first, which may be slow in accounts with many log groups
- Queries can be stopped with Ctrl+C, and the tool will attempt to stop the running query on AWS
