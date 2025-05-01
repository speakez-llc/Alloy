namespace Alloy

/// Numeric operations
[<AutoOpen>]
module Numeric =
    open Core
    
    /// Add two values
    let inline add<'T when 'T: (static member Add: 'T * 'T -> 'T)> (a: 'T) (b: 'T) : 'T =
        'T.Add(a, b)
    
    /// Subtract two values
    let inline subtract<'T when 'T: (static member Subtract: 'T * 'T -> 'T)> (a: 'T) (b: 'T) : 'T =
        'T.Subtract(a, b)
    
    /// Multiply two values
    let inline multiply<'T when 'T: (static member Multiply: 'T * 'T -> 'T)> (a: 'T) (b: 'T) : 'T =
        'T.Multiply(a, b)
    
    /// Divide two values
    let inline divide<'T when 'T: (static member Divide: 'T * 'T -> 'T)> (a: 'T) (b: 'T) : 'T =
        'T.Divide(a, b)
    
    /// Minimum of two values
    let inline min<'T when 'T: (static member Min: 'T * 'T -> 'T)> (a: 'T) (b: 'T) : 'T =
        'T.Min(a, b)
    
    /// Maximum of two values
    let inline max<'T when 'T: (static member Max: 'T * 'T -> 'T)> (a: 'T) (b: 'T) : 'T =
        'T.Max(a, b)
    
    /// Sum of collection elements
    let inline sum<'T, 'Collection when 'Collection: (static member Sum: 'Collection -> 'T)> (collection: 'Collection) : 'T =
        'Collection.Sum(collection)
    
    /// Average of collection elements
    let inline average<'T, 'Collection when 'Collection: (static member Average: 'Collection -> 'T)> (collection: 'Collection) : 'T =
        'Collection.Average(collection)
    
    /// Static implementations for numeric types
    type Int32Impl =
        static member Add(a: int32, b: int32) = a + b
        static member Subtract(a: int32, b: int32) = a - b
        static member Multiply(a: int32, b: int32) = a * b
        static member Divide(a: int32, b: int32) = a / b
        static member Min(a: int32, b: int32) = min a b
        static member Max(a: int32, b: int32) = max a b
        static member Sum(xs: int32[]) = Array.sum xs
        // For average of int array, use explicit conversion to float
        static member Average(xs: int32[]) = 
            if Array.isEmpty xs then 0
            else Array.sumBy (fun x -> float x) xs |> int |> fun n -> n / xs.Length
    
    type Float32Impl =
        static member Add(a: float32, b: float32) = a + b
        static member Subtract(a: float32, b: float32) = a - b
        static member Multiply(a: float32, b: float32) = a * b
        static member Divide(a: float32, b: float32) = a / b
        static member Min(a: float32, b: float32) = min a b
        static member Max(a: float32, b: float32) = max a b
        static member Sum(xs: float32[]) = Array.sum xs
        static member Average(xs: float32[]) = Array.average xs