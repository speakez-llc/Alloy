namespace Alloy

/// Operator symbols
module Operators =
    open Core
    open Numeric
    
    /// Pipe forward
    let (|>) x f = f x
    
    /// Pipe backward
    let (<|) f x = f x
    
    /// Composition
    let (>>) f g x = g (f x)
    
    /// Backward composition
    let (<<) g f x = g (f x)
    
    /// Addition
    let (+) a b = add a b
    
    /// Subtraction
    let (-) a b = subtract a b
    
    /// Multiplication
    let (*) a b = multiply a b
    
    /// Division
    let (/) a b = divide a b
    
    /// Equality
    let (=) a b = equals a b
    
    /// Inequality
    let (<>) a b = not_equals a b