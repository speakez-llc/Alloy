# nowarn "86" // Suppress warnings about redefining = and <> operators

namespace Alloy

/// Operator symbols
[<AutoOpen>]
module Operators =
    // Import all fsil functions
    open Fsil
    
    /// Pipe forward
    let (|>) x f = f x
    
    /// Pipe backward
    let (<|) f x = f x
    
    /// Composition
    let (>>) f g x = g (f x)
    
    /// Backward composition
    let (<<) g f x = g (f x)
    
    /// Addition
    let inline (+) a b = add a b
    
    /// Subtraction
    let inline (-) a b = subtract a b
    
    /// Multiplication
    let inline (*) a b = multiply a b
    
    /// Division
    let inline (/) a b = divide a b
    
    /// Equality
    let inline (=) a b = equals a b
    
    /// Inequality
    let inline (<>) a b = not_equals a b