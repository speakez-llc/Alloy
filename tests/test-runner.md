# Alloy Testing Guide

This document describes the canonical way to run and manage tests for the Alloy project using VSCode.

## Setup

1. Ensure you have .NET 8 SDK installed
2. Open the Alloy project folder in VSCode
3. Install the "C# Dev Kit" extension in VSCode for F# support
4. Open a terminal in the project root

## Running Tests

### From the Command Line

**Run all tests:**
```bash
dotnet test ./tests/Alloy.Tests.fsproj
```

**Run filtered tests:**
```bash
dotnet test ./tests/Alloy.Tests.fsproj --filter Core
```

You can use any test name or partial test name as a filter, such as "Core", "Numeric", or "String".

### Using VSCode Tasks

The project includes predefined tasks in `.vscode/tasks.json`:

1. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
2. Type "Run Task" and select it
3. Choose one of the following tasks:
   - **test** - Runs all tests
   - **test with filter** - Prompts for a filter and runs matching tests

### Using the Test Explorer

VSCode's Test Explorer integration allows you to:

1. View all tests in the Test Explorer panel
2. Run individual tests by clicking the "Run Test" button
3. Debug tests by clicking the "Debug Test" button
4. Run all tests in a specific file or test list

To open the Test Explorer:
1. Click on the Test Explorer icon in the activity bar
2. Tests will be automatically discovered and displayed

## Debugging Tests

### Using VSCode Debugger

1. Set breakpoints in your test code
2. Press `F5` to launch the debugger with the "Debug Tests" configuration
3. Or use "Debug Filtered Tests" to run specific tests

### Using Command Line

```bash
dotnet test ./tests/Alloy.Tests.fsproj --filter SomeTest --debug
```

## Test Structure

Each test file follows a standard pattern:

```fsharp
module Alloy.Tests.ModuleNameTests

open Expecto
open Alloy

let moduleTests =
    testList "ModuleName" [
        testCase "TestName" <| fun _ ->
            // Test implementation
    ]
    
[<Tests>]
let tests = moduleTests
```

The `[<Tests>]` attribute ensures tests are properly discovered by the test runner.

## Troubleshooting

If tests run multiple times or you encounter other issues:

1. Clean the solution: `dotnet clean`
2. Rebuild: `dotnet build`
3. Clear the test cache: `dotnet test --no-build --list-tests` followed by `dotnet test`
4. Restart VSCode or the test process

## Writing New Tests

When adding new tests:

1. Follow the standard pattern shown above
2. Use descriptive test list and test case names
3. Keep tests focused and isolated
4. Use appropriate assertions from Expecto
