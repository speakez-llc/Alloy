# Alloy

A zero-cost abstractions library for F# extending fsil with high-performance functional programming capabilities.

## Overview

Alloy builds upon the foundation laid by [fsil](https://github.com/ieviev/fsil), providing additional zero-cost abstractions for numeric operations, collections, and more. Like fsil, Alloy leverages F#'s statically resolved type parameters (SRTPs) to ensure all abstractions are resolved at compile time with no runtime overhead.

## Core Features

- **Zero-cost abstractions**: All operations are inlined and compile to the same code as direct function calls
- **No runtime overhead**: Compile-time resolution means you get high-level abstractions with low-level performance
- **Type-safe operations**: Fully leverages F#'s type system for safety and correctness
- **Trimmable**: Only pay for what you use – unused code is eliminated from your binary
- **Fable compatible**: Works seamlessly with Fable for F# to JavaScript compilation
- **Extends fsil**: Builds on the solid foundation of the fsil library's approach

## From fsil to Alloy

Alloy extends fsil's core functionality with additional operations:

| fsil provides | Alloy adds |
|---------------|------------|
| `map`, `mapi` | `add`, `subtract`, `multiply`, `divide` |
| `iter`, `iteri` | `min`, `max`, `sum`, `average` |
| `fold` | `filter`, `choose`, `find`, `tryFind` |
| `zero`, `one` | `equals`, `not_equals`, `default_value` |
| `is_some`, `value` | `ValueOption<T>` struct type |
| Basic printing | String manipulation functions |
| | Result handling functions |

## ValueOption: Zero-Allocation Option Type

Alloy provides a zero-allocation option type called `ValueOption<'T>` that serves as a high-performance alternative to F#'s built-in `Option<'T>`. This implementation is crucial for performance-critical scenarios where every allocation matters.

### Why ValueOption?

Standard F# options are reference types that allocate memory on the managed heap, which can lead to:
- Increased garbage collection (GC) pressure
- Memory fragmentation
- Cache misses due to pointer indirection
- Performance degradation in tight loops or high-frequency code paths

ValueOption solves these problems by using a struct-based implementation that stays on the stack, eliminating heap allocations entirely while maintaining F#'s familiar option semantics.

### Using ValueOption

```fsharp
open Alloy

// Creating option values
let someValue = ValueOption.Some 42
let noneValue = ValueOption<int>.None

// Checking for values using properties
if someValue.IsSome then
    printfn "Value: %d" someValue.Value

// Using module functions (similar to F#'s Option module)
let doubled = 
    someValue 
    |> ValueOption.map (fun x -> x * 2)
    |> ValueOption.defaultValue 0

// Converting between static options and standard F# options
let standardOption = ValueOption.toOption someValue
let backToStatic = ValueOption.ofOption (Some 42)
```

### How ValueOption Works

Under the hood, ValueOption uses a struct record with carefully designed properties:

```fsharp
[<Struct>]
type ValueOption<'T> =
    private { 
        hasValue: bool
        value: 'T 
    }
```

1. The `<Struct>` attribute ensures the type is a value type allocated on the stack
2. The `private` modifier forces consumers to use the type's factory methods
3. The simple internal representation enables efficient access and manipulation
4. Property accessors provide a clean, familiar API for working with option values

### Performance Considerations

ValueOption provides significant performance benefits in these scenarios:

- **High-frequency option creation**: In tight loops creating many options
- **Computation-heavy algorithms**: When processing large datasets with optional values
- **Memory-constrained environments**: In systems with limited memory or strict GC requirements
- **Real-time applications**: Where predictable, low-latency execution is critical

As a rule of thumb, prefer ValueOption when you have:
- Options in performance-critical code paths
- Collection processing with many optional values
- High-frequency option creation/manipulation
- Memory-sensitive applications

For code where performance is less critical, standard F# options might be more ergonomic due to their built-in pattern matching support.

### Integration with Result Type

ValueOption integrates seamlessly with Alloy's Result type:

```fsharp
// Convert ValueOption to Result
let result = Result.ofValueOption "Value missing" someValue

// Convert Result back to ValueOption
let option = Result.toValueOption result
```

This enables efficient, allocation-free error handling chains.

### Behind the Scenes: Static Resolution

The "static" in ValueOption refers to statically resolved type parameters (SRTPs), an F# feature that enables compile-time resolution of generic operations. This means that operations on ValueOption types have:

1. **Zero virtual method calls**: All method resolution happens at compile time
2. **Potential for inlining**: The compiler can optimize operations by inlining them
3. **No type testing or runtime reflection**: Eliminating common sources of overhead

While standard F# generics use runtime type information, statically resolved types determine everything at compile time, allowing for extremely efficient code generation.

## Installation

```
dotnet add package Alloy
```

## Basic Usage

```fsharp
open Alloy

// Numeric operations
let sum = add 5 3          // 8
let product = multiply 4 7 // 28

// Using operators
let result = 10 + 5 * 2    // 20

// Collection operations
let numbers = [|1; 2; 3; 4; 5|]
let doubled = map (fun x -> x * 2) numbers    // [|2; 4; 6; 8; 10|]
let evens = filter (fun x -> x % 2 = 0) numbers // [|2; 4|]
let total = sum numbers    // 15
let avg = average numbers  // 3

// ValueOption type - zero allocation option type
let maybeValue = ValueOption.Some 42
let hasValue = maybeValue.IsSome  // true
let value = maybeValue.Value      // 42

// String operations
let greeting = "  Hello, World!  "
let trimmed = String.trim greeting  // "Hello, World!"
let parts = String.split ',' trimmed // [|"Hello"; " World!"|]
```

## Advanced Usage

### Extending Types with Custom Operations

You can extend Alloy to work with your own types by implementing the appropriate static members:

```fsharp
// Define a custom type
type Vector2D = { X: float; Y: float }

// Implement operations
type Vector2D with
    static member Add(a: Vector2D, b: Vector2D) = 
        { X = a.X + b.X; Y = a.Y + b.Y }
    static member Subtract(a: Vector2D, b: Vector2D) = 
        { X = a.X - b.X; Y = a.Y - b.Y }
    static member Zero = { X = 0.0; Y = 0.0 }

// Now you can use Alloy's operations with your type
let v1 = { X = 3.0; Y = 4.0 }
let v2 = { X = 1.0; Y = 2.0 }
let v3 = add v1 v2      // { X = 4.0; Y = 6.0 }
let origin = zero<Vector2D>  // { X = 0.0; Y = 0.0 }
```

### Composition

Alloy functions compose naturally, just like standard F# functions:

```fsharp
let numbers = [|1..10|]

// Composition using operators
let result = numbers
             |> filter (fun x -> x % 2 = 0)  // Keep even numbers
             |> map (fun x -> x * x)         // Square them
             |> sum                          // Sum the squares

// Equivalent to: sum (map (fun x -> x * x) (filter (fun x -> x % 2 = 0) numbers))
```

## API Reference

### Numeric Operations
- `add a b` - Adds two values
- `subtract a b` - Subtracts b from a
- `multiply a b` - Multiplies two values
- `divide a b` - Divides a by b
- `min a b` - Returns the minimum of two values
- `max a b` - Returns the maximum of two values
- `sum collection` - Sums the elements of a collection
- `average collection` - Calculates the average of a collection

### Collection Operations
- `map f collection` - Maps a function over a collection
- `mapi f collection` - Maps an indexed function over a collection
- `iter f collection` - Iterates a function over a collection
- `iteri f collection` - Iterates an indexed function over a collection
- `fold folder state collection` - Folds over a collection
- `filter predicate collection` - Filters elements of a collection
- `choose chooser collection` - Chooses elements of a collection with transformation
- `find predicate collection` - Finds an element in a collection
- `tryFind predicate collection` - Tries to find an element in a collection

### Operators
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division
- `=` - Equality
- `<>` - Inequality

### ValueOption
A struct-based option type for zero-allocation operations:
- `ValueOption.Some value` - Creates a Some value
- `ValueOption<'T>.None` - Creates a None value
- `option.IsSome` - Checks if the option has a value
- `option.IsNone` - Checks if the option is None
- `option.Value` - Gets the value (throws if None)
- `ValueOption.map f opt` - Transforms the value if Some
- `ValueOption.bind f opt` - Applies a function that returns an option
- `ValueOption.defaultValue d opt` - Returns the value or default
- `ValueOption.defaultWith f opt` - Returns the value or applies a function
- `ValueOption.ofOption opt` - Converts from a standard F# option
- `ValueOption.toOption opt` - Converts to a standard F# option

### String Operations
- `String.length s` - Gets the length of a string
- `String.isEmpty s` - Checks if a string is empty
- `String.trim s` - Trims whitespace from a string
- `String.split separator s` - Splits a string by a separator
- And many more string manipulation functions

## Performance

Being a zero-cost abstraction library, Alloy operations compile to the same code as if you wrote the operations directly. This means you get the benefits of high-level abstractions with the performance of low-level code.

## License

Alloy is released under the MIT license.
