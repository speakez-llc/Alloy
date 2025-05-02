/// Span-based operations for zero-allocation memory manipulation
module Alloy.Span

open System
open Fsil
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
    let length = min source.Length destination.Length
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
    let length = min source.Length destination.Length
    
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
let inline sumSpan (span: ReadOnlySpan<'T>) : 'T =
    if span.Length = 0 then 
        Abstract.zero<'T>()  // Use fsil's zero directly
    else
        let mutable result = span.[0]
        for i = 1 to span.Length - 1 do
            result <- Numeric.add result span.[i]
        result

/// Computes the average of elements in a Span
let inline averageSpan< ^T when ^T : (static member Zero : ^T) and 
                            ^T : (static member (+) : ^T * ^T -> ^T) and
                            ^T : (static member (/) : ^T * int -> ^T)> 
    (span: ReadOnlySpan< ^T>) : ^T =
    if span.Length = 0 then 
        (^T : (static member Zero : ^T) ())
    else
        let mutable sum = span.[0]
        for i = 1 to span.Length - 1 do
            sum <- sum + span.[i]
        sum / span.Length
    
// --------------------------------------------------
// Helper functions
// --------------------------------------------------

/// Convert an integer to a generic numeric type
/// Convert an integer to a generic numeric type
let inline private fromInteger< ^T when ^T : (static member Zero : ^T) and 
                                     ^T : (static member (+) : ^T * ^T -> ^T) and
                                     ^T : (static member One : ^T)> 
    (value: int) : ^T =
    let mutable result = (^T : (static member Zero : ^T) ())
    let one = (^T : (static member One : ^T) ())
    for _ = 1 to value do
        result <- result + one
    result