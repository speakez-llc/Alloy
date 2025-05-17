module Alloy.Span

open System
open Alloy.Core
open Alloy.Numerics

/// <summary>Creates a Span from an array.</summary>
/// <param name="array">The source array.</param>
/// <typeparam name="T">The type of elements in the array.</typeparam>
/// <returns>A Span that represents the entire array.</returns>
let inline asSpan (array: 'T[]) : Span<'T> = 
    Span<'T>(array)

/// <summary>Creates a Span from a subrange of an array.</summary>
/// <param name="array">The source array.</param>
/// <param name="start">The zero-based index of the first element in the Span.</param>
/// <param name="length">The number of elements in the Span.</param>
/// <typeparam name="T">The type of elements in the array.</typeparam>
/// <returns>A Span that represents a portion of the array.</returns>
let inline sliceSpan (array: 'T[]) (start: int) (length: int) : Span<'T> =
    Span<'T>(array, start, length)

/// <summary>Creates a ReadOnlySpan from an array.</summary>
/// <param name="array">The source array.</param>
/// <typeparam name="T">The type of elements in the array.</typeparam>
/// <returns>A ReadOnlySpan that represents the entire array.</returns>
let inline asReadOnlySpan (array: 'T[]) : ReadOnlySpan<'T> = 
    ReadOnlySpan<'T>(array)

/// <summary>Creates a ReadOnlySpan from a subrange of an array.</summary>
/// <param name="array">The source array.</param>
/// <param name="start">The zero-based index of the first element in the ReadOnlySpan.</param>
/// <param name="length">The number of elements in the ReadOnlySpan.</param>
/// <typeparam name="T">The type of elements in the array.</typeparam>
/// <returns>A ReadOnlySpan that represents a portion of the array.</returns>
let inline sliceReadOnlySpan (array: 'T[]) (start: int) (length: int) : ReadOnlySpan<'T> =
    ReadOnlySpan<'T>(array, start, length)

/// <summary>Maps a function over elements in a ReadOnlySpan and writes results to a destination Span.</summary>
/// <param name="mapper">The mapping function to apply to each element.</param>
/// <param name="source">The source ReadOnlySpan.</param>
/// <param name="destination">The destination Span where results will be written.</param>
/// <typeparam name="T">The type of elements in the source span.</typeparam>
/// <typeparam name="U">The type of elements in the destination span.</typeparam>
let inline mapSpan (mapper: 'T -> 'U) (source: ReadOnlySpan<'T>) (destination: Span<'U>) : unit =
    let minLength a b = if a < b then a else b
    let length = minLength source.Length destination.Length
    for i = 0 to length - 1 do
        destination.[i] <- mapper source.[i]

/// <summary>Fills all elements of a Span with the specified value.</summary>
/// <param name="value">The value to fill the span with.</param>
/// <param name="span">The span to fill.</param>
/// <typeparam name="T">The type of elements in the span.</typeparam>
let inline fillSpan (value: 'T) (span: Span<'T>) : unit =
    span.Fill(value)

/// <summary>Clears all elements in a Span (sets them to default value).</summary>
/// <param name="span">The span to clear.</param>
/// <typeparam name="T">The type of elements in the span.</typeparam>
let inline clearSpan (span: Span<'T>) : unit =
    span.Clear()

/// <summary>Copies elements from source Span to destination Span.</summary>
/// <param name="source">The source ReadOnlySpan.</param>
/// <param name="destination">The destination Span.</param>
/// <typeparam name="T">The type of elements in the spans.</typeparam>
let inline copySpan (source: ReadOnlySpan<'T>) (destination: Span<'T>) : unit =
    source.CopyTo(destination)

/// <summary>Filters elements in a ReadOnlySpan based on a predicate and writes results to a destination Span.</summary>
/// <param name="predicate">The predicate function used to test elements.</param>
/// <param name="source">The source ReadOnlySpan.</param>
/// <param name="destination">The destination Span where matched elements will be written.</param>
/// <typeparam name="T">The type of elements in the spans.</typeparam>
/// <returns>The number of elements written to the destination span.</returns>
let inline filterSpan (predicate: 'T -> bool) (source: ReadOnlySpan<'T>) (destination: Span<'T>) : int =
    let mutable count = 0
    let minLength a b = if a < b then a else b
    let length = minLength source.Length destination.Length
    
    for i = 0 to source.Length - 1 do
        if predicate source.[i] && count < length then
            destination.[count] <- source.[i]
            count <- count + 1
            
    count

/// <summary>Applies a function that combines an element with the accumulator over a ReadOnlySpan.</summary>
/// <param name="folder">The function to update the state given the current state and element.</param>
/// <param name="state">The initial state.</param>
/// <param name="span">The span to fold over.</param>
/// <typeparam name="State">The type of the state.</typeparam>
/// <typeparam name="T">The type of elements in the span.</typeparam>
/// <returns>The final accumulated state.</returns>
let inline foldSpan (folder: 'State -> 'T -> 'State) (state: 'State) (span: ReadOnlySpan<'T>) : 'State =
    let mutable result = state
    for i = 0 to span.Length - 1 do
        result <- folder result span.[i]
    result

/// <summary>Sums all elements in a Span.</summary>
/// <param name="span">The span containing elements to sum.</param>
/// <typeparam name="T">The type of elements in the span, must support zero and addition.</typeparam>
/// <returns>The sum of all elements in the span.</returns>
let inline sumSpan<'T when 'T: (static member Zero: 'T) 
                      and (^T or Internal.BasicOps or Internal.MeasureOps): (static member Add: ^T * ^T -> ^T)> 
    (span: ReadOnlySpan<'T>) : 'T =
    
    if span.Length = 0 then 
        zero<'T>
    else
        let mutable result = span.[0]
        for i = 1 to span.Length - 1 do
            result <- add result span.[i]
        result

/// <summary>Computes the average of elements in a Span.</summary>
/// <param name="span">The span containing elements to average.</param>
/// <typeparam name="T">The type of elements in the span, must support zero, addition, and division.</typeparam>
/// <returns>The average of all elements in the span.</returns>
let inline averageSpan<'T when 'T: (static member Zero: 'T) 
                        and (^T or Internal.BasicOps or Internal.MeasureOps): (static member Add: ^T * ^T -> ^T)
                        and (^T or Internal.BasicOps or Internal.MeasureOps or Internal.MeasureMeasureOps): (static member Divide: ^T * ^T -> ^T)> 
    (span: ReadOnlySpan<'T>) : 'T =
    
    if span.Length = 0 then 
        zero<'T>
    else
        let mutable sum = span.[0]
        for i = 1 to span.Length - 1 do
            sum <- add sum span.[i]
            
        match box sum with
        | :? int as intSum -> box (intSum / span.Length) :?> 'T
        | :? float as floatSum -> box (floatSum / float span.Length) :?> 'T
        | :? int64 as longSum -> box (longSum / int64 span.Length) :?> 'T
        | :? float32 as floatSum -> box (floatSum / float32 span.Length) :?> 'T
        | _ -> 
            let divisor =
                match box 0 with
                | :? int -> box span.Length :?> 'T
                | :? float -> box (float span.Length) :?> 'T
                | :? int64 -> box (int64 span.Length) :?> 'T
                | :? float32 -> box (float32 span.Length) :?> 'T
                | _ -> box span.Length :?> 'T
                    
            divide sum divisor