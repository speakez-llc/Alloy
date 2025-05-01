namespace Alloy

/// Zero-cost Option type that avoids allocation
[<Struct>]
type ValueOption<'T> =
    | ValueSome of value:'T
    | ValueNone
    
    // Instance properties
    member this.IsSome = 
        match this with 
        | ValueSome _ -> true 
        | ValueNone -> false
    
    member this.Value = 
        match this with
        | ValueSome x -> x
        | ValueNone -> failwith "No value"
        
    // These static members allow ValueOption to work with fsil's functions
    static member Zero = ValueNone
    static member DefaultValue = ValueNone
    
    static member Some(x: 'T) = ValueSome x
    static member None() = ValueNone
    
    static member HasValue (opt: ValueOption<'T>) = opt.IsSome
    
    static member Map ((f: 'T -> 'U, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> ValueSome (f x)
        | ValueNone -> ValueNone
    
    static member MapIndexed ((f: int -> 'T -> 'U, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> ValueSome (f 0 x)
        | ValueNone -> ValueNone
    
    static member Iterate ((f: 'T -> unit, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> f x
        | ValueNone -> ()
    
    static member IterateIndexed ((f: int -> 'T -> unit, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> f 0 x
        | ValueNone -> ()
    
    static member Bind ((f: 'T -> ValueOption<'U>, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x -> f x
        | ValueNone -> ValueNone
        
    static member Find ((predicate: 'T -> bool, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x when predicate x -> x
        | _ -> failwith "No matching element found"
    
    static member TryFind ((predicate: 'T -> bool, opt: ValueOption<'T>)) =
        match opt with
        | ValueSome x when predicate x -> ValueSome x
        | _ -> ValueNone
    
    // Conversion between F# Option and ValueOption
    static member OfOption (opt: 'T option) =
        match opt with
        | Some x -> ValueSome x
        | None -> ValueNone
        
    static member ToOption (opt: ValueOption<'T>) =
        match opt with
        | ValueSome x -> Some x
        | ValueNone -> None