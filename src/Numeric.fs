/// Numeric operations for the Alloy library
module Alloy.Numeric

open Alloy.Core

// --------------------------------------------------
// Type definitions for numeric operations
// --------------------------------------------------

/// Internal implementation for numeric operations
module Internal =
    [<AbstractClass; Sealed>]
    type Add =
        static member inline Add((a: int, b: int)) = a + b
        static member inline Add((a: float, b: float)) = a + b
        static member inline Add((a: int64, b: int64)) = a + b
        static member inline Add((a: uint64, b: uint64)) = a + b
        static member inline Add((a: float32, b: float32)) = a + b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Add) : (static member Add: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
    
    [<AbstractClass; Sealed>]
    type Subtract =
        static member inline Subtract((a: int, b: int)) = a - b
        static member inline Subtract((a: float, b: float)) = a - b
        static member inline Subtract((a: int64, b: int64)) = a - b
        static member inline Subtract((a: uint64, b: uint64)) = a - b
        static member inline Subtract((a: float32, b: float32)) = a - b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Subtract) : (static member Subtract: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
    
    [<AbstractClass; Sealed>]
    type Multiply =
        static member inline Multiply((a: int, b: int)) = a * b
        static member inline Multiply((a: float, b: float)) = a * b
        static member inline Multiply((a: int64, b: int64)) = a * b
        static member inline Multiply((a: uint64, b: uint64)) = a * b
        static member inline Multiply((a: float32, b: float32)) = a * b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Multiply) : (static member Multiply: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
    
    [<AbstractClass; Sealed>]
    type Divide =
        static member inline Divide((a: int, b: int)) = a / b
        static member inline Divide((a: float, b: float)) = a / b
        static member inline Divide((a: int64, b: int64)) = a / b
        static member inline Divide((a: uint64, b: uint64)) = a / b
        static member inline Divide((a: float32, b: float32)) = a / b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Divide) : (static member Divide: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
    
    [<AbstractClass; Sealed>]
    type Min =
        static member inline Min((a: int, b: int)) = min a b
        static member inline Min((a: float, b: float)) = min a b
        static member inline Min((a: int64, b: int64)) = min a b
        static member inline Min((a: uint64, b: uint64)) = min a b
        static member inline Min((a: float32, b: float32)) = min a b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Min) : (static member Min: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
    
    [<AbstractClass; Sealed>]
    type Max =
        static member inline Max((a: int, b: int)) = max a b
        static member inline Max((a: float, b: float)) = max a b
        static member inline Max((a: int64, b: int64)) = max a b
        static member inline Max((a: uint64, b: uint64)) = max a b
        static member inline Max((a: float32, b: float32)) = max a b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Max) : (static member Max: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
    
    [<AbstractClass; Sealed>]
    type Sum =
        static member inline Sum(xs: int[]) = 
            let mutable sum = 0
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: float[]) = 
            let mutable sum = 0.0
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: int64[]) = 
            let mutable sum = 0L
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
            
        static member inline Sum(xs: float32[]) = 
            let mutable sum = 0.0f
            for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
            sum
        
        static member inline Invoke (collection: ^Collection) : ^T =
            let inline call_1 (collection: ^Collection) =
                ((^Collection or Sum) : (static member Sum: ^Collection -> ^T) collection)
            call_1(collection)
    
    [<AbstractClass; Sealed>]
    type Average =
        static member inline Average(xs: int[]) = 
            if xs.Length = 0 then 0
            else
                let mutable sum = 0
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / xs.Length
                
        static member inline Average(xs: float[]) = 
            if xs.Length = 0 then 0.0
            else
                let mutable sum = 0.0
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / float xs.Length
                
        static member inline Average(xs: int64[]) = 
            if xs.Length = 0 then 0L
            else
                let mutable sum = 0L
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / int64 xs.Length
                
        static member inline Average(xs: float32[]) = 
            if xs.Length = 0 then 0.0f
            else
                let mutable sum = 0.0f
                for i = 0 to xs.Length - 1 do sum <- sum + xs.[i]
                sum / float32 xs.Length
        
        static member inline Invoke (collection: ^Collection) : ^T =
            let inline call_1 (collection: ^Collection) =
                ((^Collection or Average) : (static member Average: ^Collection -> ^T) collection)
            call_1(collection)

// --------------------------------------------------
// Public API
// --------------------------------------------------

/// Add two values
let inline add a b = Internal.Add.Invoke a b

/// Subtract b from a
let inline subtract a b = Internal.Subtract.Invoke a b

/// Multiply two values
let inline multiply a b = Internal.Multiply.Invoke a b

/// Divide a by b
let inline divide a b = Internal.Divide.Invoke a b

/// Return the minimum of two values
let inline min a b = Internal.Min.Invoke a b

/// Return the maximum of two values
let inline max a b = Internal.Max.Invoke a b

/// Sum all elements in a collection
let inline sum collection = Internal.Sum.Invoke collection

/// Calculate the average of elements in a collection
let inline average collection = Internal.Average.Invoke collection