namespace Alloy

/// Core module with fundamental operations
[<AutoOpen>]
module Core =
    // Import all fsil functions
    open Fsil
    
    /// Default value for a type
    let inline default_value<'T when 'T: (static member DefaultValue: 'T)> : 'T = 'T.DefaultValue
    
    /// Empty instance of a collection type
    let inline empty<'Collection when 'Collection: (static member Empty: 'Collection)> : 'Collection = 'Collection.Empty
    
    /// Equality comparison
    let inline equals<'T when 'T: (static member Equals: 'T * 'T -> bool)> (a: 'T) (b: 'T) : bool =
        'T.Equals(a, b)
    
    /// Inequality comparison
    let inline not_equals<'T when 'T: (static member Equals: 'T * 'T -> bool)> (a: 'T) (b: 'T) : bool =
        not ('T.Equals(a, b))
    
    // Module for type implementations and extensions
    [<AutoOpen>]
    module TypeImplementations =
        // Static method implementations for primitive types
        type Int =
            static member DefaultValue = 0
            static member Equals (a: int, b: int) = a = b
            
        type Float =
            static member DefaultValue = 0.0
            static member Equals (a: float, b: float) = a = b
            
        type String =
            static member DefaultValue = ""
            static member Equals (a: string, b: string) = a = b
            
        type Bool =
            static member DefaultValue = false
            static member Equals (a: bool, b: bool) = a = b