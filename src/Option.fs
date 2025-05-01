namespace Alloy

/// Zero-cost Option type that avoids allocation
[<Struct>]
type ValueOption<'T> =
    | ValueSome of value:'T
    | ValueNone
    
    static member Zero = ValueNone
    static member HasValue (opt: ValueOption<'T>) = 
        match opt with 
        | ValueSome _ -> true 
        | ValueNone -> false
    
    static member Value (opt: ValueOption<'T>) = 
        match opt with
        | ValueSome x -> x
        | ValueNone -> failwith "No value"
    
    static member Some (x: 'T) = ValueSome x
    static member None<'T>() : ValueOption<'T> = ValueNone // Changed to a method
    
    static member Map (f: 'T -> 'U, opt: ValueOption<'T>) =
        match opt with
        | ValueSome x -> ValueSome (f x)
        | ValueNone -> ValueNone
    
    static member Bind (f: 'T -> ValueOption<'U>, opt: ValueOption<'T>) =
        match opt with
        | ValueSome x -> f x
        | ValueNone -> ValueNone
        
    // Add extensions for better interop
    static member OfOption (opt: 'T option) =
        match opt with
        | Some x -> ValueSome x
        | None -> ValueNone
        
    static member ToOption (opt: ValueOption<'T>) =
        match opt with
        | ValueSome x -> Some x
        | ValueNone -> None