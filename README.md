# CloudWatch Insights Query Tool

<p align="center">
  <img src="assets/logo.png" alt="CloudWatch Insights Query Tool Logo" width="200">
</p>

A command-line utility for executing and retrieving CloudWatch Insights queries with flexible log group selection and time range options.

## Features

- Execute CloudWatch Insights queries directly from the command line
- Sub-command structure for organized functionality (`run`, `library`, `query`)
- Query library system for saving and reusing common queries
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
- `--query-name NAME` - Use a saved query from your library
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

Manage your collection of saved queries.

##### `ciqt library list`
List all available queries in your library.

```
ciqt library list [--query-library DIR]
```

##### `ciqt library save <name>`
Save a query to your library.

```
ciqt library save <name> [--query TEXT | --query-file FILE | --query-name NAME] [--query-library DIR]
```

##### `ciqt library delete <name>`
Delete a query from your library.

```
ciqt library delete <name> [--query-library DIR]
```

##### `ciqt library show <name>`
Display the content of a saved query.

```
ciqt library show <name> [--query-library DIR]
```

#### `ciqt query` - Show Query Content

Display query content without execution.

```
ciqt query [--query TEXT | --query-file FILE | --query-name NAME] [--query-library DIR]
```

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

Use a saved query from your library:
```bash
ciqt run --query-name aws/lambda/errors --log-groups /aws/lambda/my-function --since PT24H
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

## Notes

- When using `--log-group-glob` or `--log-group-regex`, the tool retrieves all log groups first, which may be slow in accounts with many log groups
- Queries can be stopped with Ctrl+C, and the tool will attempt to stop the running query on AWS
