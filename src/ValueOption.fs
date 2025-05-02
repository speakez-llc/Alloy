namespace Alloy

/// A struct-based option type that avoids allocation
[<Struct>]
type ValueOption<'T> =
    | ValueSome of value:'T
    | ValueNone
    
    // --------------------------------------------------
    // Instance properties
    // --------------------------------------------------
    
    /// Gets whether the option has a value
    member this.IsSome = 
        match this with 
        | ValueSome _ -> true 
        | ValueNone -> false
    
    /// Gets whether the option has no value
    member this.IsNone =
        match this with
        | ValueSome _ -> false
        | ValueNone -> true
    
    /// Gets the option value (throws if None)
    member this.Value = 
        match this with
        | ValueSome x -> x
        | ValueNone -> failwith "ValueOption does not have a value"
    
    /// Attempts to get the value, returns default if None
    member this.GetValueOrDefault(defaultValue: 'T) =
        match this with
        | ValueSome x -> x
        | ValueNone -> defaultValue
    
    /// Attempts to get the value, uses generator function if None
    member this.GetValueOrElse(generator: unit -> 'T) =
        match this with
        | ValueSome x -> x
        | ValueNone -> generator()
    
    // --------------------------------------------------
    // Static members for fsil compatibility
    // --------------------------------------------------
    
    /// Returns the None value
    static member Zero = ValueNone
    
    /// Returns the default value (None)
    static member DefaultValue = ValueNone
    
    /// Creates a Some value
    static member Some(x: 'T) = ValueSome x
    
    /// Creates a None value
    static member None = ValueNone
    
    /// Checks if the option has a value
    static member HasValue (opt: ValueOption<'T>) = opt.IsSome
    
    // --------------------------------------------------
    // Static methods for processing
    // --------------------------------------------------
    
    /// Maps a function over a ValueOption
    static member Map ((f: 'T -> 'U, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> ValueSome (f x)
        | ValueNone -> ValueNone
    
    /// Maps a function with index over a ValueOption
    static member MapIndexed ((f: int -> 'T -> 'U, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> ValueSome (f 0 x)
        | ValueNone -> ValueNone
    
    /// Applies a function to the value if present
    static member Iterate ((f: 'T -> unit, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> f x
        | ValueNone -> ()
    
    /// Applies an indexed function to the value if present
    static member IterateIndexed ((f: int -> 'T -> unit, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> f 0 x
        | ValueNone -> ()
    
    /// Applies a binding function to the value if present
    static member Bind ((f: 'T -> ValueOption<'U>, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> f x
        | ValueNone -> ValueNone
    
    /// Finds a value matching a predicate
    static member Find ((predicate: 'T -> bool, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x when predicate x -> x
        | _ -> failwith "No matching element found"
    
    /// Tries to find a value matching a predicate
    static member TryFind ((predicate: 'T -> bool, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x when predicate x -> ValueSome x
        | _ -> ValueNone
    
    // --------------------------------------------------
    // Conversion methods
    // --------------------------------------------------
    
    /// Converts from F# Option to ValueOption
    static member OfOption (opt: 'T option) =
        match opt with
        | Some x -> ValueSome x
        | None -> ValueNone
    
    /// Converts from ValueOption to F# Option
    static member ToOption (opt: ValueOption<'T>) =
        match opt with
        | ValueSome x -> Some x
        | ValueNone -> None
    
    /// Converts to string representation
    override this.ToString() =
        match this with
        | ValueSome x -> $"ValueSome({x})"
        | ValueNone -> "ValueNone"

/// Module with functions for working with ValueOption
module ValueOption =
    /// Creates a ValueSome containing the given value
    let inline some (value: 'T) = ValueSome value
    
    /// Creates a ValueNone
    let inline none<'T> () = ValueNone
    
    /// Checks if the ValueOption has a value
    let inline isSome (opt: ValueOption<'T>) = opt.IsSome
    
    /// Checks if the ValueOption has no value
    let inline isNone (opt: ValueOption<'T>) = opt.IsNone
    
    /// Gets the value or throws if ValueNone
    let inline value (opt: ValueOption<'T>) = opt.Value
    
    /// Gets the value or returns the specified default value if ValueNone
    let inline defaultValue (defaultValue: 'T) (opt: ValueOption<'T>) =
        opt.GetValueOrDefault(defaultValue)
        
    /// Gets the value or executes the generator function if ValueNone
    let inline defaultWith (generator: unit -> 'T) (opt: ValueOption<'T>) =
        opt.GetValueOrElse(generator)
    
    /// Maps a function over a ValueOption
    let inline map (mapping: 'T -> 'U) (opt: ValueOption<'T>) : ValueOption<'U> =
        match opt with
        | ValueSome x -> ValueSome (mapping x)
        | ValueNone -> ValueNone
    
    /// Applies a function to the value if present
    let inline iter (action: 'T -> unit) (opt: ValueOption<'T>) : unit =
        match opt with
        | ValueSome x -> action x
        | ValueNone -> ()
    
    /// Applies a binding function to the value if present
    let inline bind (binder: 'T -> ValueOption<'U>) (opt: ValueOption<'T>) : ValueOption<'U> =
        match opt with
        | ValueSome x -> binder x
        | ValueNone -> ValueNone
    
    /// Converts from F# Option to ValueOption
    let inline ofOption (opt: 'T option) : ValueOption<'T> =
        match opt with
        | Some x -> ValueSome x
        | None -> ValueNone
    
    /// Converts from ValueOption to F# Option
    let inline toOption (opt: ValueOption<'T>) : 'T option =
        match opt with
        | ValueSome x -> Some x
        | ValueNone -> None