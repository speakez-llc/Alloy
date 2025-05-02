/// Operator symbols for the Alloy library
[<AutoOpen>]
module Alloy.Operators

#nowarn "86" // Suppress warnings about redefining = and <> operators

// Import core functionality
open Alloy.Core
open Alloy.Numerics

// --------------------------------------------------
// Function composition operators
// --------------------------------------------------

/// Forward pipe operator
let inline (|>) x f = f x

/// Backward pipe operator
let inline (<|) f x = f x

/// Forward composition operator
let inline (>>) f g x = g (f x)

/// Backward composition operator
let inline (<<) g f x = g (f x)

// --------------------------------------------------
// Arithmetic operators
// --------------------------------------------------

/// Addition operator
let inline (+) a b = add a b

/// Subtraction operator
let inline (-) a b = subtract a b

/// Multiplication operator
let inline (*) a b = multiply a b

/// Division operator
let inline (/) a b = divide a b

// --------------------------------------------------
// Comparison operators
// --------------------------------------------------

/// Equality operator
let inline (=) a b = equals a b

/// Inequality operator
let inline (<>) a b = not_equals a b