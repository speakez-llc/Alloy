namespace Alloy

#nowarn "86" // Suppress warnings about redefining = and <> operators

open Alloy.Core
open Alloy.Numerics

/// <summary>
/// Provides zero-cost operators through statically resolved type parameters.
/// All functions are exposed through the AutoOpen attribute, making them accessible
/// when opening the Alloy namespace.
/// </summary>
[<AutoOpen>]
module Operators =
    // Function composition operators remain the same
    let inline (|>) x f = f x
    let inline (<|) f x = f x
    let inline (>>) f g x = g (f x)
    let inline (<<) g f x = g (f x)

    // More generic arithmetic operators with improved type flexibility
    let inline (+) a b = add a b
    let inline (-) a b = subtract a b
    let inline (*) a b = multiply a b
    let inline (/) a b = divide a b

    // Generic comparison operators
    let inline (=) a b = equals a b
    let inline (<>) a b = not_equals a b
    let inline (<) a b = ((^a or ^b) : (static member op_LessThan: ^a * ^b -> bool) (a, b))
    let inline (>) a b = ((^a or ^b) : (static member op_GreaterThan: ^a * ^b -> bool) (a, b))
    let inline (<=) a b = ((^a or ^b) : (static member op_LessThanOrEqual: ^a * ^b -> bool) (a, b))
    let inline (>=) a b = ((^a or ^b) : (static member op_GreaterThanOrEqual: ^a * ^b -> bool) (a, b))

    // Generic function application operators for more flexible composition
    let inline (<*>) f x = ((^f or ^x) : (static member Apply: ^f * ^x -> 'r) (f, x))
    let inline (<!>) f x = ((^f or ^x) : (static member Map: ^f * ^x -> 'r) (f, x))