namespace Alloy

/// A zero-allocation, statically resolved option type
[<Struct>]
type StaticOption<'T> =
    private { 
        hasValue: bool
        value: 'T 
    }
    
    // Instance properties
    member this.IsSome = this.hasValue
    member this.IsNone = not this.hasValue
    member this.Value = if this.hasValue then this.value else failwith "No value"
    
    // Static factory methods - used to create instances
    static member Some(value: 'T) = { hasValue = true; value = value }
    static member None = { hasValue = false; value = Unchecked.defaultof<'T> }
    
    // Static members for SRTP support
    static member Zero = StaticOption<'T>.None
    static member DefaultValue = StaticOption<'T>.None
    
    // String representation
    override this.ToString() = 
        if this.hasValue then $"Some({this.value})" else "None"

/// Functions for working with the StaticOption<'T> type
module StaticOption =
    // Active pattern defined at module level for pattern matching
    let (|SomePattern|NonePattern|) (opt: StaticOption<'T>) =
        if opt.IsSome then SomePattern opt.Value else NonePattern
    
    /// Creates a Some value
    let inline some v = StaticOption.Some v
    
    /// Returns the None value
    let inline none<'T> = StaticOption<'T>.None
    
    /// Checks if an option is Some
    let inline isSome (opt: StaticOption<'T>) = opt.IsSome
    
    /// Checks if an option is None
    let inline isNone (opt: StaticOption<'T>) = opt.IsNone
    
    /// Gets the value of a Some option, throws if None
    let inline value (opt: StaticOption<'T>) = opt.Value
    
    /// Returns the value if Some, or the default value if None
    let inline defaultValue defaultValue (opt: StaticOption<'T>) =
        if opt.IsSome then opt.Value else defaultValue
        
    /// Returns the value if Some, or calls the generator function if None
    let inline defaultWith generator (opt: StaticOption<'T>) =
        if opt.IsSome then opt.Value else generator()
    
    /// Transforms the value inside an option
    let inline map mapping (opt: StaticOption<'T>) =
        if opt.IsSome then StaticOption.Some(mapping opt.Value) 
        else StaticOption<_>.None
    
    /// Transforms an option with a function that returns an option
    let inline bind binder (opt: StaticOption<'T>) =
        if opt.IsSome then binder opt.Value
        else StaticOption<_>.None
    
    /// Converts a standard F# option to a StaticOption
    let inline ofOption (opt: 'T option) =
        if Option.isSome opt then 
            StaticOption.Some (Option.get opt)
        else 
            StaticOption<'T>.None
        
    /// Converts a StaticOption to a standard F# option
    let inline toOption (opt: StaticOption<'T>) =
        if opt.IsSome then Some opt.Value else None