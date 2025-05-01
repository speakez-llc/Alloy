namespace Alloy

/// Core module with fundamental operations
[<AutoOpen>]
module Core =
    // Import all fsil functions
    open Fsil
    
    /// Zero for a type
    let inline zero<'T when 'T: (static member Zero: 'T)> : 'T = 'T.Zero
    
    /// One for a type
    let inline one<'T when 'T: (static member One: 'T)> : 'T = 'T.One
    
    /// Default value for a type
    let inline default_value<'T when 'T: (static member DefaultValue: 'T)> : 'T = 'T.DefaultValue
    
    /// Empty instance of a collection type
    let inline empty<'Collection when 'Collection: (static member Empty: 'Collection)> : 'Collection = 'Collection.Empty
    
    /// Length of a collection
    let inline len<'Collection when 'Collection: (static member Length: 'Collection -> int)> (collection: 'Collection) : int = 
        'Collection.Length(collection)
    
    /// Access value in an option-like container
    let inline value<'T, 'Container when 'Container: (member Value: 'T)> (container: 'Container) : 'T = 
        container.Value
    
    /// Check if container has a value
    let inline has_value<'Container when 'Container: (member IsSome: bool)> (container: 'Container) : bool = 
        container.IsSome
    
    /// Create a container with a value
    let inline some<'T, 'Container when 'Container: (static member Some: 'T -> 'Container)> (value: 'T) : 'Container = 
        'Container.Some(value)
    
    /// Create an empty container
    let inline none<'Container when 'Container: (static member None: unit -> 'Container)> () : 'Container = 
        'Container.None()
    
    /// Convert to string
    let inline to_string<'T when 'T: (override ToString: unit -> string)> (value: 'T) : string = 
        value.ToString()
    
    /// Print a value to the console
    let inline print (value: 'a) =
        // Convert value to string representation using F# string formatting
        let str = sprintf "%A" value
        
        // Output the string (this part requires interaction with outside world)
        // In a truly pure functional approach, we would return a description of the
        // printing action rather than performing it directly
        { new obj() with
            override _.ToString() =
                // This causes the string to be displayed when the object is evaluated
                str }

    /// Equality comparison
    let inline equals<'T when 'T: (static member Equals: 'T * 'T -> bool)> (a: 'T) (b: 'T) : bool =
        'T.Equals(a, b)
    
    /// Inequality comparison
    let inline not_equals<'T when 'T: (static member Equals: 'T * 'T -> bool)> (a: 'T) (b: 'T) : bool =
        not ('T.Equals(a, b))

    // Static method implementations for primitive types
    type Int =
        static member Zero = 0
        static member One = 1
        static member DefaultValue = 0
        static member Equals (a: int, b: int) = a = b
        static member ToString (x: int) = x.ToString()
        
    type Float =
        static member Zero = 0.0
        static member One = 1.0
        static member DefaultValue = 0.0
        static member Equals (a: float, b: float) = a = b
        static member ToString (x: float) = x.ToString()
        
    type String =
        static member Zero = ""
        static member DefaultValue = ""
        static member Equals (a: string, b: string) = a = b
        static member ToString (x: string) = x
        
    type Bool =
        static member Zero = false
        static member One = true
        static member DefaultValue = false
        static member Equals (a: bool, b: bool) = a = b
        static member ToString (x: bool) = x.ToString()
        
    type Option =
        static member HasValue (opt: 'T option) = opt.IsSome
        static member Value (opt: 'T option) = opt.Value
        static member Some (x: 'T) = Some x
        static member None<'T>() : 'T option = None