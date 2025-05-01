namespace Alloy

/// Core module with fundamental operations
module Core =

    // Import all fsil functions
    open fsil
    /// Zero for a type
    let zero<'T> = ((^T or ^Unit) : (static member Zero : 'T) ())
    
    /// One for a type
    let one<'T> = ((^T or ^Unit) : (static member One : 'T) ())
    
    /// Default value for a type
    let default_value<'T> = ((^T or ^Unit) : (static member DefaultValue : 'T) ())
    
    /// Empty instance of a collection type
    let empty<'Collection> = ((^Collection or ^Unit) : (static member Empty : 'Collection) ())
    
    /// Length of a collection
    let len<'Collection> = ((^Collection or ^Unit) : (static member Length : 'Collection -> int) ())
    
    /// Access value in an option-like container
    let value<'T, 'Container> (container: 'Container) =
        ((^Container or ^T) : (static member Value : 'Container -> 'T) container)
    
    /// Check if container has a value
    let has_value<'Container> (container: 'Container) =
        ((^Container or ^Unit) : (static member HasValue : 'Container -> bool) container)
    
    /// Create a container with a value
    let some<'T, 'Container> (value: 'T) =
        ((^Container or ^T) : (static member Some : 'T -> 'Container) value)
    
    /// Create an empty container
    let none<'Container> () =
        ((^Container or ^Unit) : (static member None : 'Container) ())
    
    /// Convert to string
    let to_string<'T> (value: 'T) =
    ((^T or ^Unit) : (static member ToString : 'T -> string) value)
    
    /// Print a value to the console
    let print (value: 'a) =
        // Convert value to string representation using F# string formatting
        let str = sprintf "%A" value
        
        // Output the string (this part requires interaction with outside world)
        // In a truly pure functional approach, we would return a description of the
        // printing action rather than performing it directly
        { new obj() with
            override _.ToString() =
                // This causes the string to be displayed when the object is evaluated
                str }

    // Type class for equality comparisons
    type IEquatable<'T> =
        static member Equals : 'T -> 'T -> bool

    /// Equality comparison
    let equals<'T when 'T :> IEquatable<'T>> (a: 'T) (b: 'T) =
        ((^T or ^Unit) : (static member Equals : 'T -> 'T -> bool) a b)
    
    /// Inequality comparison
    let not_equals<'T when 'T :> IEquatable<'T>> (a: 'T) (b: 'T) =
        not (equals a b)

    // Static method implementations for primitive types
    type Int =
        static member Zero = 0
        static member One = 1
        static member DefaultValue = 0
        static member Equals (a: int) (b: int) = a = b
        static member ToString (x: int) = x.ToString()
        
    type Float =
        static member Zero = 0.0
        static member One = 1.0
        static member DefaultValue = 0.0
        static member Equals (a: float) (b: float) = a = b
        static member ToString (x: float) = x.ToString()
        
    type String =
        static member Zero = ""
        static member DefaultValue = ""
        static member Equals (a: string) (b: string) = a = b
        static member ToString (x: string) = x
        
    type Bool =
        static member Zero = false
        static member One = true
        static member DefaultValue = false
        static member Equals (a: bool) (b: bool) = a = b
        static member ToString (x: bool) = x.ToString()
        
    type Option =
        static member HasValue (opt: 'T option) = opt.IsSome
        static member Value (opt: 'T option) = opt.Value
        static member Some (x: 'T) = Some x
        static member None<'T> : 'T option = None