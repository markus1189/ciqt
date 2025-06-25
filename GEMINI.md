# GEMINI.md

This document is a guide for the Gemini AI assistant to effectively work with the `ciqt` repository. It synthesizes information from the project's `README.md` and `CLAUDE.md`.

## 1. Project Overview

`ciqt` (CloudWatch Insights Query Tool) is a Haskell command-line utility for executing CloudWatch Insights queries. It features flexible log group selection, multiple time range options, and a dual query library system (local and AWS).

### 1.1. Technical Architecture

- **Language**: Haskell
- **Build System**: Nix Flakes
- **Package Manager**: Cabal
- **Core Principles**:
    - **Modular Design**: A core library with a minimal executable wrapper.
    - **Functional Paradigm**: Immutable data structures, pure functions separated from IO.
    - **Lens-Heavy**: Extensive use of the `lens` library for data access and manipulation (`^.`, `.~`, `&`).
    - **Type-Safe**: Strong typing using custom ADTs for domain modeling.
    - **Resource Management**: `ResourceT` and `bracket` patterns ensure safe AWS resource handling (e.g., stopping queries).

### 1.2. Module Structure

The core logic is separated into focused modules within `src/Ciqt/`:

- `Ciqt.Types`: Defines core data types (`AppArgs`, `RunArgs`, `LogGroupsArg`, `TimeRange`, `QueryArg`).
- `Ciqt.CLI`: Handles command-line argument parsing using `optparse-applicative`.
- `Ciqt.AWS`: Manages AWS SDK (`amazonka`) integration, including credential discovery and API calls.
- `Ciqt.Query`: Orchestrates query resolution, execution, and result processing.
- `Ciqt.Library`: Implements the local and AWS query library management logic.
- `Ciqt.Utils`: Contains pure helper functions (e.g., path expansion, JSON parsing).
- `Ciqt`: The main library module that dispatches commands and orchestrates the application flow.
- `Main`: The thin executable wrapper that calls into the `Ciqt` library.

## 2. Development Workflow

### 2.1. Environment Setup

The project is managed entirely through Nix.

- **Enter development shell**: Provides access to all tools (GHC, HLS, cabal, ormolu).
  ```bash
  nix develop
  ```
- **Start an interactive REPL (GHCi)**:
  ```bash
  nix develop --command ghci
  ```

### 2.2. Building and Running

- **Build the executable**:
  ```bash
  nix build
  ```
  The result is symlinked to `./result/bin/ciqt`.

- **Run the executable**:
  ```bash
  nix run . -- <args>
  ```
  Example: `nix run . -- --help`

### 2.3. Code Formatting

The project uses `ormolu` for code formatting.

- **Check formatting**:
  ```bash
  nix develop --command ormolu --mode check src/**/*.hs test/**/*.hs
  ```
- **Apply formatting**:
  ```bash
  nix develop --command ormolu --mode inplace src/**/*.hs test/**/*.hs
  ```

### 2.4. Testing

The test suite uses the **Tasty** framework (`tasty-hunit` for unit tests, `tasty-golden` for CLI output regression tests).

- **Run all tests**:
  ```bash
  nix develop --command cabal test
  ```
- **Run tests with verbose output**:
  ```bash
  nix develop --command cabal test --test-show-details=streaming
  ```
- **Update golden files**: If CLI help text or other golden-tested output changes intentionally, run:
  ```bash
  nix develop --command cabal test --test-options="--accept"
  ```
- **Run a specific test**: Use the `-p` flag to specify a pattern.
  ```bash
  nix develop --command cabal test --test-options="-p '/main-help-output/'"
  ```

## 3. Architecture and Code Patterns

### 3.1. Lens Usage

Lenses are used pervasively for safe and concise field access.

- **Read a value**: `args ^. runArgs . runArgsQuery`
- **Set a value**: `env & AWS.logger .~ customLogger`
- **Set a `Maybe` value**: `query & startQuery_limit ?~ n`
- **Chain operations**: `args & field1 .~ val1 & field2 .~ val2`

### 3.2. Resource Management

AWS resources that require explicit cleanup (like a running query) are managed with `bracket`.

```haskell
-- Abstracted pattern in src/Ciqt/Query.hs
bracket
  (AWS.send env startQuery)          -- Acquire resource (start query)
  (esp -> AWS.send env (stopQuery (resp ^. startQueryResponse_queryId))) -- Release (stop query)
  (esp -> pollForResults env resp)  -- Use resource (poll for results)
```

### 3.3. Error Handling

- A top-level `try @SomeException` in `Main.hs` catches all exceptions.
- User-facing errors are printed to `stderr`, and the program exits with a non-zero exit code.
- In pure code, `Maybe` and `Either` are preferred over exceptions.

## 4. Common Modification Tasks

### 4.1. Adding a New CLI Option to the `run` command

1.  **Update Type**: Add the new field to `RunArgs` in `src/Ciqt/Types.hs`.
    ```haskell
    data RunArgs = RunArgs { _runArgsNewOption :: Maybe Text, ... }
    ```
2.  **Generate Lens**: Ensure `makeLenses ''RunArgs` is present to generate the `runArgsNewOption` lens.
3.  **Update Parser**: Add a parser for the new option in `runArgsParser` in `src/Ciqt/CLI.hs`.
    ```haskell
    runArgsParser = RunArgs
      <$> ...
      <*> optional (strOption (long "new-option" <> ...))
    ```
4.  **Use Value**: Access the value in `src/Ciqt.hs` using the lens.
    ```haskell
    let maybeNewOpt = args ^. runArgs . runArgsNewOption
    ```

### 4.2. Adding a New Log Group Selection Method

1.  **Extend ADT**: Add a new constructor to `LogGroupsArg` in `src/Ciqt/Types.hs`.
    ```haskell
    data LogGroupsArg = ... | NewLogGroupMethod Text
    ```
2.  **Update Logic**: Add a case to the `calculateLogGroups` function in `src/Ciqt/AWS.hs` to handle the new constructor.
3.  **Update Parser**: Add a corresponding CLI option and parser in `src/Ciqt/CLI.hs`.

## 5. AWS Integration

### 5.1. Authentication

Authentication uses the standard `amazonka` credential chain (environment variables, EC2 instance profile, shared credentials file `~/.aws/credentials`).

### 5.2. Required IAM Permissions

**Basic Query Execution:**
- `logs:StartQuery`
- `logs:GetQueryResults`
- `logs:StopQuery`
- `logs:DescribeLogGroups`

**AWS Query Library Functionality:**
- `logs:DescribeQueryDefinitions`
- `logs:PutQueryDefinition`
- `logs:DeleteQueryDefinition`

## 6. Query Library System

`ciqt` supports two types of query libraries that can be synchronized.

### 6.1. Local Library

- **Location**: `~/.ciqt/queries/` by default, override with `--query-library`.
- **Format**: Plain text files with a `.query` extension.
- **Structure**: Subdirectories are supported for organization (e.g., `aws/lambda/errors.query`).
- **Management**: Use `ciqt library list|save|delete|show`.

### 6.2. AWS Library

- **Source**: CloudWatch Logs Insights "Saved queries".
- **Management**: Use `ciqt library aws-list|aws-download|aws-upload|aws-delete`.
- **Syncing**: `ciqt library sync` shows a side-by-side comparison of local and AWS libraries.
