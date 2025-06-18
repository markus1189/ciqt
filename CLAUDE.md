# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `ciqt` (CloudWatch Insights Query Tool) - a Haskell command-line utility for executing and retrieving CloudWatch Insights queries with flexible log group selection and time range options.

## Build System

This project uses Nix for build management:
- `nix build` - Build the project (creates `result` symlink to built executable)
- `nix develop` - Enter development shell with all dependencies
- `nix develop --command ghci` - Start GHCi with all dependencies loaded

The project uses `package.yaml` (hpack format) instead of traditional `.cabal` files.

## Architecture

- **Single-file application**: All logic is in `src/Main.hs`
- **AWS Integration**: Uses `amazonka` library for CloudWatch Logs API calls
- **Query Library**: Supports saved queries in `~/.ciqt/queries/` directory with `.query` extension
- **Command-line parsing**: Uses `optparse-applicative` for argument parsing
- **Lens-heavy**: Extensive use of lens operators for data manipulation

### Key Data Types
- `LogGroupsArg`: Union type for different log group selection methods (comma-separated, pattern, prefix, glob, regex)
- `TimeRange`: Absolute or relative time ranges for queries
- `QueryArg`: Query source (file, string, or library)
- `AppArgs`: Main application configuration

### Core Functions
- `calculateLogGroups`: Resolves log group specifications to actual log group names
- `executeQuery`: Handles query execution with proper resource cleanup
- `calculateQuery`: Loads query text from various sources (file, library, direct string)

## AWS Dependencies

The project uses standard amazonka packages from nixpkgs. The `package.yaml` specifies minimum versions (`>= 2.0`) for amazonka packages to ensure compatibility.

## Query Library

The tool supports a comprehensive query library system:
- Queries stored as `.query` files in `~/.ciqt/queries/` (or custom directory)
- Directory tree organization supported (e.g., `aws/lambda/errors.query`)
- Query management via CLI: `--list-queries`, `--save-query`, `--delete-query`
- Queries referenced by path: `--query-name aws/lambda/errors`
- Library operations work independently of time/log group parameters