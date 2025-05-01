namespace Alloy

/// Numeric operations
module Numeric =
    open Core
    
    /// Add two values
    let add<'T> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Add : 'T * 'T -> 'T) (a, b))
    
    /// Subtract two values
    let subtract<'T> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Subtract : 'T * 'T -> 'T) (a, b))
    
    /// Multiply two values
    let multiply<'T> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Multiply : 'T * 'T -> 'T) (a, b))
    
    /// Divide two values
    let divide<'T> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Divide : 'T * 'T -> 'T) (a, b))
    
    /// Minimum of two values
    let min<'T> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Min : 'T * 'T -> 'T) (a, b))
    
    /// Maximum of two values
    let max<'T> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Max : 'T * 'T -> 'T) (a, b))
    
    /// Sum of collection elements
    let sum<'T, 'Collection> (collection: 'Collection) =
        ((^Collection or ^T) : (static member Sum : 'Collection -> 'T) collection)
    
    /// Average of collection elements
    let average<'T, 'Collection> (collection: 'Collection) =
        ((^Collection or ^T) : (static member Average : 'Collection -> 'T) collection)
    
    /// Static implementations for numeric types
    type Int32Impl =
        static member Add(a: int32, b: int32) = a + b
        static member Subtract(a: int32, b: int32) = a - b
        static member Multiply(a: int32, b: int32) = a * b
        static member Divide(a: int32, b: int32) = a / b
        static member Min(a: int32, b: int32) = min a b
        static member Max(a: int32, b: int32) = max a b
        static member Sum(xs: int32[]) = Array.sum xs
        static member Average(xs: int32[]) = Array.average xs
    
    type Float32Impl =
        static member Add(a: float32, b: float32) = a + b
        static member Subtract(a: float32, b: float32) = a - b
        static member Multiply(a: float32, b: float32) = a * b
        static member Divide(a: float32, b: float32) = a / b
        static member Min(a: float32, b: float32) = min a b
        static member Max(a: float32, b: float32) = max a b
        static member Sum(xs: float32[]) = Array.sum xs
        static member Average(xs: float32[]) = Array.average xs