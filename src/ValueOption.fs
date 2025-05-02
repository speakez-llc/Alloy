namespace Alloy

[<Struct>]
type ValueOption<'T> =
    | ValueSome of value:'T
    | ValueNone
    
    // Instance properties (keep as-is)
    member this.IsSome = match this with ValueSome _ -> true | _ -> false
    member this.IsNone = match this with ValueNone -> true | _ -> false
    member this.Value = match this with ValueSome x -> x | _ -> failwith "No value"
    
    // Simplified static members
    static member Zero = ValueNone
    static member DefaultValue = ValueNone

    // ToString remains the same
    override this.ToString() = 
        match this with
        | ValueSome x -> $"ValueSome({x})"
        | ValueNone -> "ValueNone"

// Improved module
module ValueOption =
    let inline some v = ValueSome v
    let inline none<'T> = ValueNone : ValueOption<'T>
    
    let inline isSome (opt: ValueOption<'T>) = opt.IsSome
    let inline isNone (opt: ValueOption<'T>) = opt.IsNone
    let inline value (opt: ValueOption<'T>) = opt.Value
    
    let inline defaultValue defaultValue (opt: ValueOption<'T>) =
        match opt with ValueSome x -> x | ValueNone -> defaultValue
        
    let inline defaultWith generator (opt: ValueOption<'T>) =
        match opt with ValueSome x -> x | ValueNone -> generator()
    
    // Cleaner function signatures
    let inline map mapping (opt: ValueOption<'T>) =
        match opt with ValueSome x -> ValueSome(mapping x) | ValueNone -> ValueNone
    
    let inline bind binder (opt: ValueOption<'T>) =
        match opt with ValueSome x -> binder x | ValueNone -> ValueNone
    
    // Conversion helpers
    let inline ofOption (opt: 'T option) =
        match opt with Some x -> ValueSome x | None -> ValueNone
        
    let inline toOption (opt: ValueOption<'T>) =
        match opt with ValueSome x -> Some x | ValueNone -> None