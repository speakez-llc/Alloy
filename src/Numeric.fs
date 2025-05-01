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
    [<AutoOpen>]
    module NumericImplementations =
        type Int32Impl =
            static member Add(a: int32, b: int32) = a + b
            static member Subtract(a: int32, b: int32) = a - b
            static member Multiply(a: int32, b: int32) = a * b
            static member Divide(a: int32, b: int32) = a / b
            static member Min(a: int32, b: int32) = min a b
            static member Max(a: int32, b: int32) = max a b
        
        type Float32Impl =
            static member Add(a: float32, b: float32) = a + b
            static member Subtract(a: float32, b: float32) = a - b
            static member Multiply(a: float32, b: float32) = a * b
            static member Divide(a: float32, b: float32) = a / b
            static member Min(a: float32, b: float32) = min a b
            static member Max(a: float32, b: float32) = max a b
        
        type ArrayImpl =
            static member Sum(xs: int[]) = Array.sum xs
            static member Sum(xs: float[]) = Array.sum xs
            static member Average(xs: int[]) = 
                if Array.isEmpty xs then 0
                else Array.sum xs / xs.Length
            static member Average(xs: float[]) = Array.average xs
        
        // Extensions for primitive types
        module Extensions =
            // We need to put extensions in a module when extending types
            // that are defined in different assemblies
            type System.Int32 with
                static member Add(a: int, b: int) = Int32Impl.Add(a, b)
                static member Subtract(a: int, b: int) = Int32Impl.Subtract(a, b)
                static member Multiply(a: int, b: int) = Int32Impl.Multiply(a, b)
                static member Divide(a: int, b: int) = Int32Impl.Divide(a, b)
                static member Min(a: int, b: int) = Int32Impl.Min(a, b)
                static member Max(a: int, b: int) = Int32Impl.Max(a, b)
            
            type System.Double with
                static member Add(a: float, b: float) = a + b
                static member Subtract(a: float, b: float) = a - b
                static member Multiply(a: float, b: float) = a * b
                static member Divide(a: float, b: float) = a / b
                static member Min(a: float, b: float) = min a b
                static member Max(a: float, b: float) = max a b
                
            type System.Array with 
                static member Sum(xs: int[]) = ArrayImpl.Sum(xs)
                static member Sum(xs: float[]) = ArrayImpl.Sum(xs)
                static member Average(xs: int[]) = ArrayImpl.Average(xs)
                static member Average(xs: float[]) = ArrayImpl.Average(xs)