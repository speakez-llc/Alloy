module Alloy.Span

open System
open Alloy.Core
open Alloy.Numeric

// --------------------------------------------------
// Span creation functions
// --------------------------------------------------

/// Create a Span from an array
let inline asSpan (array: 'T[]) : Span<'T> = 
    Span<'T>(array)

/// Create a Span from a subrange of an array
let inline sliceSpan (array: 'T[]) (start: int) (length: int) : Span<'T> =
    Span<'T>(array, start, length)

/// Create a ReadOnlySpan from an array
let inline asReadOnlySpan (array: 'T[]) : ReadOnlySpan<'T> = 
    ReadOnlySpan<'T>(array)

/// Create a ReadOnlySpan from a subrange of an array
let inline sliceReadOnlySpan (array: 'T[]) (start: int) (length: int) : ReadOnlySpan<'T> =
    ReadOnlySpan<'T>(array, start, length)

// --------------------------------------------------
// Non-allocating Span operations
// --------------------------------------------------

/// Maps a function over elements in a Span and writes results to a destination Span
/// This operation doesn't allocate a new Span
let inline mapSpan (mapper: 'T -> 'U) (source: ReadOnlySpan<'T>) (destination: Span<'U>) : unit =
    // Create a local helper function for finding minimum of two integers
    // This avoids the ambiguity between Numeric.min and F#'s built-in min
    let minLength a b = if a < b then a else b
    let length = minLength source.Length destination.Length
    for i = 0 to length - 1 do
        destination.[i] <- mapper source.[i]

/// Fills all elements of a Span with the specified value
let inline fillSpan (value: 'T) (span: Span<'T>) : unit =
    span.Fill(value)

/// Clears all elements in a Span (sets them to default value)
let inline clearSpan (span: Span<'T>) : unit =
    span.Clear()

/// Copies elements from source Span to destination Span
let inline copySpan (source: ReadOnlySpan<'T>) (destination: Span<'T>) : unit =
    source.CopyTo(destination)

/// Filters elements in a Span based on a predicate and writes results to a destination Span
/// Returns the number of elements written to the destination
let inline filterSpan (predicate: 'T -> bool) (source: ReadOnlySpan<'T>) (destination: Span<'T>) : int =
    let mutable count = 0
    // Create a local helper function for finding minimum of two integers
    let minLength a b = if a < b then a else b
    let length = minLength source.Length destination.Length
    
    for i = 0 to source.Length - 1 do
        if predicate source.[i] && count < length then
            destination.[count] <- source.[i]
            count <- count + 1
            
    count

/// Applies a function that combines an element with the accumulator and updates it in-place
let inline foldSpan (folder: 'State -> 'T -> 'State) (state: 'State) (span: ReadOnlySpan<'T>) : 'State =
    let mutable result = state
    for i = 0 to span.Length - 1 do
        result <- folder result span.[i]
    result

/// Sums all elements in a Span
let inline sumSpan<'T when 'T: (static member Zero: 'T) and (^T or Alloy.Numeric.Internal.Add): (static member Add: ^T * ^T -> ^T)> 
    (span: ReadOnlySpan<'T>) : 'T =
    if span.Length = 0 then 
        zero<'T>
    else
        let mutable result = span.[0]
        for i = 1 to span.Length - 1 do
            result <- Numeric.add result span.[i]
        result

/// Computes the average of elements in a Span
let inline averageSpan<'T when 'T: (static member Zero: 'T) 
                        and (^T or Alloy.Numeric.Internal.Add): (static member Add: ^T * ^T -> ^T)
                        and (^T or Alloy.Numeric.Internal.Divide): (static member Divide: ^T * ^T -> ^T)> 
    (span: ReadOnlySpan<'T>) : 'T =
    if span.Length = 0 then 
        zero<'T>
    else
        let mutable sum = span.[0]
        for i = 1 to span.Length - 1 do
            sum <- Numeric.add sum span.[i]
            
        // For numeric primitives - direct handling
        match box sum with
        | :? int as intSum -> box (intSum / span.Length) :?> 'T
        | :? float as floatSum -> box (floatSum / float span.Length) :?> 'T
        | :? int64 as longSum -> box (longSum / int64 span.Length) :?> 'T
        | :? float32 as floatSum -> box (floatSum / float32 span.Length) :?> 'T
        | _ -> 
            // For custom types with Divide operation
            // Create a divisor of the appropriate type
            let divisor =
                match box 0 with
                | :? int -> box span.Length :?> 'T
                | :? float -> box (float span.Length) :?> 'T
                | :? int64 -> box (int64 span.Length) :?> 'T
                | :? float32 -> box (float32 span.Length) :?> 'T
                | _ -> 
                    // Convert length to the same type as T
                    // This is a fallback and may not work for all types
                    box span.Length :?> 'T
                    
            Numeric.divide sum divisor