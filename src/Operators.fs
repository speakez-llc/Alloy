# nowarn "86" // Suppress warnings about redefining = and <> operators

namespace Alloy

/// Operator symbols
[<AutoOpen>]
module Operators =
    open Core
    open Alloy.Numeric
    
    /// Pipe forward
    let (|>) x f = f x
    
    /// Pipe backward
    let (<|) f x = f x
    
    /// Composition
    let (>>) f g x = g (f x)
    
    /// Backward composition
    let (<<) g f x = g (f x)
    
    /// Addition
    let inline (+) (a: ^T) (b: ^T) : ^T when ^T : (static member Add: ^T * ^T -> ^T) = add a b
    
    /// Subtraction
    let inline (-) (a: ^T) (b: ^T) : ^T when ^T : (static member Subtract: ^T * ^T -> ^T) = subtract a b
    
    /// Multiplication
    let inline (*) (a: ^T) (b: ^T) : ^T when ^T : (static member Multiply: ^T * ^T -> ^T) = multiply a b
    
    /// Division
    let inline (/) (a: ^T) (b: ^T) : ^T when ^T : (static member Divide: ^T * ^T -> ^T) = divide a b
    
    /// Equality
    let inline (=) (a: ^T) (b: ^T) : bool when ^T : (static member Equals: ^T * ^T -> bool) = equals a b
    
    /// Inequality
    let inline (<>) (a: ^T) (b: ^T) : bool when ^T : (static member Equals: ^T * ^T -> bool) = not_equals a b