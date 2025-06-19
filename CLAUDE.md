# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `ciqt` (CloudWatch Insights Query Tool) - a Haskell command-line utility for executing and retrieving CloudWatch Insights queries with flexible log group selection and time range options.

## Development Setup

### Build System

This project uses Nix for build management:
- `nix build` - Build the project (creates `result` symlink to built executable)
- `nix develop` - Enter development shell with all dependencies
- `nix develop --command ghci` - Start GHCi with all dependencies loaded
- `nix run . -- <args>` - Run the built binary with arguments
- `nix shell` - Add ciqt to PATH temporarily

The project uses `package.yaml` (hpack format) instead of traditional `.cabal` files.

### Code Formatting

- **Formatter**: `ormolu` is used for code formatting
- Available in development shell: `nix develop --command ormolu`
- Formatting is automatically applied via development tooling

### Testing

Currently no formal test suite is configured. Testing is done through:
- Manual CLI testing with various argument combinations
- Integration testing with actual AWS CloudWatch Logs

## Architecture Overview

### Design Principles
- **Single-file application**: All logic is in `src/Main.hs`
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

### Core Functions
- `calculateLogGroups`: Resolves log group specifications to actual log group names
- `executeQuery`: Handles query execution with proper resource cleanup and retry logic
- `calculateQuery`: Loads query text from various sources (file, library, direct string, AWS)
- `mainProgram`: Entry point with error handling and command routing

## AWS Integration

### Dependencies
The project uses standard amazonka packages from nixpkgs:
- `amazonka-core` - Core AWS functionality
- `amazonka-cloudwatch-logs` - CloudWatch Logs API bindings
- The `package.yaml` specifies minimum versions (`>= 2.0`) for compatibility

### Authentication
- Uses standard AWS credential chain (environment variables, profiles, IAM roles)
- Automatically discovers AWS environment through `amazonka`
- Supports all standard AWS authentication methods

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

### Common Development Tasks

1. **Building and Testing**:
   ```bash
   nix build                           # Build the project
   nix run . -- --help                # Test basic functionality
   nix run . -- library list          # Test library functionality
   ```

2. **Development Shell**:
   ```bash
   nix develop                         # Enter dev environment
   ghci                               # Start GHCi REPL
   ormolu --mode inplace src/Main.hs  # Format code
   ```

3. **Testing Different Features**:
   ```bash
   # Test query execution (requires AWS credentials)
   nix run . -- run --query 'fields @timestamp | limit 10' --log-groups '/aws/lambda/test' --since PT1H
   
   # Test library management
   nix run . -- library save test-query --query 'fields @timestamp'
   nix run . -- library list
   ```

### Troubleshooting

- **Build failures**: Check that all dependencies are available in nixpkgs
- **AWS authentication**: Verify AWS credentials are configured (`aws sts get-caller-identity`)
- **Query library**: Ensure `~/.ciqt/queries/` directory exists and is writable
- **Completion scripts**: The flake generates completion scripts but they're not essential for functionality

### Common Modifications

When making changes:
1. Update types in the data definitions section if changing CLI structure
2. Update parsers when adding new command-line options
3. Use lens operators consistently for data access
4. Maintain resource cleanup patterns for AWS operations
5. Follow the existing error handling patterns with `ExitFailure`