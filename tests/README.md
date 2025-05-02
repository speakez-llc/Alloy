# Alloy Test Suite

This directory contains comprehensive tests for the Alloy library, ensuring that all functionality works as expected.

## Test Structure

The test suite is organized by module, with each module having its own dedicated test file:

- **CoreTests.fs**: Tests for the core functionality, including basic operations, option handling, and collection operations.
- **NumericTests.fs**: Tests for numeric operations on both primitive and custom types.
- **OperatorTests.fs**: Tests for operator overloads and function composition.
- **ResultTests.fs**: Tests for the Result module's error handling capabilities.
- **SpanTests.fs**: Tests for span-based operations that minimize allocations.
- **StringTests.fs**: Tests for string manipulation functions.
- **ValueOptionTests.fs**: Tests for the struct-based ValueOption type.
- **TestHelpers.fs**: Common helper functions used across the test suite.

## Running Tests

You can run the entire test suite using the .NET CLI:

```bash
dotnet test
```

To run specific test categories:

```bash
dotnet test --filter "Core"       # Run just Core tests
dotnet test --filter "Numeric"    # Run just Numeric tests
dotnet test --filter "String"     # Run just String tests
```

## Adding New Tests

When adding new functionality to Alloy, follow these guidelines for testing:

1. **Use the appropriate test file**: Add tests to the relevant module's test file.
2. **Create test lists**: Organize tests into logical groups using `testList`.
3. **Test edge cases**: Include tests for edge cases such as empty collections, null inputs, etc.
4. **Verify against standard F# functions**: When applicable, verify that Alloy's functions match the behavior of standard F# functions (e.g., comparing `filter` with `Array.filter`).
5. **Test custom types**: Ensure that operations work not just with primitive types but also with custom types.
6. **Benchmark allocation-sensitive operations**: For operations that are designed to minimize allocations, consider adding benchmarks.

## Test Helpers

The `TestHelpers.fs` file contains:

- **Custom types**: `Vector2D` and `Person` for testing generic operations.
- **Assertion helpers**: Extended assertion functions for working with specific types.
- **Span testing utilities**: Helpers for testing spans and read-only spans.

## Continuous Integration

Add tests to your CI pipeline to ensure regressions are caught early:

```yaml
# Example GitHub Actions workflow step
- name: Run tests
  run: dotnet test --configuration Release
```