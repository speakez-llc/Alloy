/// Core operations and functions for the Alloy library
module Alloy.Core

// Import core fsil functionality
open Fsil

// --------------------------------------------------
// Core functions reexported from fsil
// --------------------------------------------------

/// Applies a function to each element of a collection
let inline iter f x = iter f x 

/// Applies a function with the index to each element of a collection
let inline iteri f x = iteri f x

/// Maps a function over a collection, producing a new collection
let inline map f x = map f x

/// Maps a function with index over a collection, producing a new collection
let inline mapi f x = mapi f x

/// Gets the length of a collection
let inline len source = len source

/// Gets the zero value for a type
let inline zero<'a when 'a: (static member Zero: 'a)> = Abstract.zero<'a>

/// Gets the one/unit value for a type
let inline one<'a when 'a: (static member One: 'a)> = Abstract.one<'a>

/// Gets the default value for a type
let inline default_value<'t when (^t or Internal.Default): (static member Default: (^t -> unit) -> ^t)> = 
    Abstract._default<'t>

/// Uses a fallback function if a value is None
let inline default_with f x = default_with f x

/// Converts a value to string
let inline string x = string x

/// Print a value to stdout
let inline print x = print x

// --------------------------------------------------
// Option functions reexported from fsil
// --------------------------------------------------

/// Checks if an option contains a value
let inline is_some x = is_some x

/// Checks if an option is None
let inline is_none x = is_none x

/// Gets the value from an option, throws if None
let inline value x = value x

/// Creates a None option
let inline none<'a when 'a: (static member None: 'a)> = Abstract.none<'a>

/// Wraps a value in Some
let inline some x = some x

// --------------------------------------------------
// Additional core functions
// --------------------------------------------------

/// Fold implementation for collections
/// This function applies an accumulator function to each element of a collection
let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (collection: 'Collection) =
    ((^Collection or ^State) : (static member Fold : ('State -> 'T -> 'State) * 'State * 'Collection -> 'State)
        (folder, state, collection))

/// Filter implementation for arrays - note: this allocates a new array
/// This function returns a new array containing only the elements that satisfy the predicate
let inline filter (predicate: 'T -> bool) (source: 'T[]) =
    let mutable count = 0
    // First pass: count matching elements
    for i = 0 to source.Length - 1 do
        if predicate source.[i] then
            count <- count + 1
    
    // Allocate result array of exact size
    let result = Array.zeroCreate count
    
    // Second pass: fill result array
    let mutable j = 0
    for i = 0 to source.Length - 1 do
        if predicate source.[i] then
            result.[j] <- source.[i]
            j <- j + 1
    
    result

/// Choose implementation for arrays - note: this allocates a new array
/// This function applies a chooser function to each element and collects the Some results
let inline choose (chooser: 'T -> 'U option) (source: 'T[]) =
    // First determine exact result size
    let mutable count = 0
    for i = 0 to source.Length - 1 do
        match chooser source.[i] with
        | Some _ -> count <- count + 1
        | None -> ()
    
    // Allocate result array of exact size
    let result = Array.zeroCreate count
    
    // Fill result array
    let mutable j = 0
    for i = 0 to source.Length - 1 do
        match chooser source.[i] with
        | Some value -> 
            result.[j] <- value
            j <- j + 1
        | None -> ()
    
    result

/// Find an element in a collection that matches a predicate
let inline find (predicate: 'T -> bool) (collection: 'Collection) =
    ((^Collection) : (static member Find : ('T -> bool) * 'Collection -> 'T)
        (predicate, collection))

/// Try to find an element in a collection that matches a predicate
let inline tryFind (predicate: 'T -> bool) (collection: 'Collection) =
    ((^Collection) : (static member TryFind : ('T -> bool) * 'Collection -> 'T option)
        (predicate, collection))

/// Generic equality check
let inline equals (a: 'T) (b: 'T) : bool =
    ((^T) : (static member Equals : 'T * 'T -> bool) (a, b))

/// Generic inequality check
let inline not_equals (a: 'T) (b: 'T) : bool = 
    not (equals a b)

/// Gets the minimum of two values using SRTP
let inline min<'T when ^T : (static member Min : ^T * ^T -> ^T)> (a: 'T) (b: 'T) : 'T = 
    ((^T) : (static member Min : ^T * ^T -> ^T) (a, b))

/// Gets the maximum of two values using SRTP  
let inline max<'T when ^T : (static member Max : ^T * ^T -> ^T)> (a: 'T) (b: 'T) : 'T =
    ((^T) : (static member Max : ^T * ^T -> ^T) (a, b))