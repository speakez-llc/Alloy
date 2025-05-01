module Alloy

// Import all fsil functionality directly
open Fsil

// This uses the existing fsil pattern to make extensions
// using the same style of SRTP/inline function pattern

// Internal implementations that follow fsil's pattern exactly
module Internal =
    [<AbstractClass; Sealed>]
    type Add =
        static member inline Add((a: int, b: int)) = a + b
        static member inline Add((a: float, b: float)) = a + b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Add) : (static member Add: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
            
    [<AbstractClass; Sealed>]
    type Subtract =
        static member inline Subtract((a: int, b: int)) = a - b
        static member inline Subtract((a: float, b: float)) = a - b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Subtract) : (static member Subtract: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
            
    [<AbstractClass; Sealed>]
    type Multiply =
        static member inline Multiply((a: int, b: int)) = a * b
        static member inline Multiply((a: float, b: float)) = a * b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Multiply) : (static member Multiply: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
            
    [<AbstractClass; Sealed>]
    type Divide =
        static member inline Divide((a: int, b: int)) = a / b
        static member inline Divide((a: float, b: float)) = a / b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Divide) : (static member Divide: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
            
    [<AbstractClass; Sealed>]
    type Min =
        static member inline Min((a: int, b: int)) = min a b
        static member inline Min((a: float, b: float)) = min a b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Min) : (static member Min: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
            
    [<AbstractClass; Sealed>]
    type Max =
        static member inline Max((a: int, b: int)) = max a b
        static member inline Max((a: float, b: float)) = max a b
        
        static member inline Invoke (a: ^T) (b: ^T) : ^T =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Max) : (static member Max: ^T * ^T -> ^T) (a, b))
            call_2(a, b)
            
    [<AbstractClass; Sealed>]
    type Sum =
        static member inline Sum((xs: int[])) = Array.sum xs
        static member inline Sum((xs: float[])) = Array.sum xs
        
        static member inline Invoke (collection: ^Collection) : ^T =
            let inline call_1 (collection: ^Collection) =
                ((^Collection or Sum) : (static member Sum: ^Collection -> ^T) collection)
            call_1(collection)
            
    [<AbstractClass; Sealed>]
    type Average =
        static member inline Average((xs: int[])) = 
            if Array.isEmpty xs then 0
            else Array.sum xs / xs.Length
        static member inline Average((xs: float[])) = Array.average xs
        
        static member inline Invoke (collection: ^Collection) : ^T =
            let inline call_1 (collection: ^Collection) =
                ((^Collection or Average) : (static member Average: ^Collection -> ^T) collection)
            call_1(collection)
            
    [<AbstractClass; Sealed>]
    type DefaultValue =
        static member inline DefaultValue((dummy: int)) = 0
        static member inline DefaultValue((dummy: float)) = 0.0
        static member inline DefaultValue((dummy: string)) = ""
        static member inline DefaultValue((dummy: bool)) = false
        
        static member inline Invoke<^T when (^T or DefaultValue) : (static member DefaultValue: ^T -> ^T)> () : ^T =
            let mutable dummy = Unchecked.defaultof<^T>
            ((^T or DefaultValue) : (static member DefaultValue: ^T -> ^T) dummy)
    
    // Additional array functions beyond what fsil provides
    [<AbstractClass; Sealed>]
    type Filter =
        static member inline Filter((xs: 'T[], predicate: 'T -> bool)) = Array.filter predicate xs
        
        static member inline Invoke (predicate: 'T -> bool) (collection: 'Collection) : 'Collection =
            let inline call_2 (predicate: 'T -> bool, collection: 'Collection) =
                ((^Collection or Filter) : (static member Filter: ('T -> bool) * 'Collection -> 'Collection) (predicate, collection))
            call_2(predicate, collection)
    
    [<AbstractClass; Sealed>]
    type Choose =
        static member inline Choose((xs: 'T[], chooser: 'T -> 'U option)) = Array.choose chooser xs
        
        static member inline Invoke (chooser: 'T -> 'U option) (collection: 'Collection) : 'Collection =
            let inline call_2 (chooser: 'T -> 'U option, collection: 'Collection) =
                ((^Collection or Choose) : (static member Choose: ('T -> 'U option) * 'Collection -> 'Collection) (chooser, collection))
            call_2(chooser, collection)
    
    [<AbstractClass; Sealed>]
    type Find =
        static member inline Find((xs: 'T[], predicate: 'T -> bool)) = Array.find predicate xs
        
        static member inline Invoke (predicate: 'T -> bool) (collection: 'Collection) : 'T =
            let inline call_2 (predicate: 'T -> bool, collection: 'Collection) =
                ((^Collection or Find) : (static member Find: ('T -> bool) * 'Collection -> 'T) (predicate, collection))
            call_2(predicate, collection)
    
    [<AbstractClass; Sealed>]
    type TryFind =
        static member inline TryFind((xs: 'T[], predicate: 'T -> bool)) = Array.tryFind predicate xs
        
        static member inline Invoke (predicate: 'T -> bool) (collection: 'Collection) : 'T option =
            let inline call_2 (predicate: 'T -> bool, collection: 'Collection) =
                ((^Collection or TryFind) : (static member TryFind: ('T -> bool) * 'Collection -> 'T option) (predicate, collection))
            call_2(predicate, collection)
    
    [<AbstractClass; Sealed>]
    type Equals =
        static member inline Equals((a: int, b: int)) = a = b
        static member inline Equals((a: float, b: float)) = a = b
        static member inline Equals((a: string, b: string)) = a = b
        static member inline Equals((a: bool, b: bool)) = a = b
        
        static member inline Invoke (a: ^T) (b: ^T) : bool =
            let inline call_2 (a: ^T, b: ^T) =
                ((^T or Equals) : (static member Equals: ^T * ^T -> bool) (a, b))
            call_2(a, b)

// The public API
[<AbstractClass; Sealed; AutoOpen>]
type Abstract =
    // Re-export fsil functions directly
    static member inline iter (f: 't -> unit) (x: ^I) = Fsil.Abstract.iter f x
    static member inline iteri (f: int -> 't -> unit) (x: ^I) = Fsil.Abstract.iteri f x
    static member inline map (f: 't -> 'u) (x: ^I) = Fsil.Abstract.map f x
    static member inline mapi (f: int -> 't -> 'u) (x: ^I) = Fsil.Abstract.mapi f x
    static member inline len(source: 't) = Fsil.Abstract.len source
    static member inline zero<^a when ^a: (static member Zero: ^a)>() = Fsil.Abstract.zero<^a>
    static member inline one<^a>() = Fsil.Abstract.one<^a>
    static member inline _default<^t>() = Fsil.Abstract._default<^t>()
    static member inline to_string(x) = Fsil.Abstract.to_string x
    
    // New Alloy functions
    static member inline add (a: ^T) (b: ^T) = Internal.Add.Invoke a b
    static member inline subtract (a: ^T) (b: ^T) = Internal.Subtract.Invoke a b
    static member inline multiply (a: ^T) (b: ^T) = Internal.Multiply.Invoke a b
    static member inline divide (a: ^T) (b: ^T) = Internal.Divide.Invoke a b
    static member inline min (a: ^T) (b: ^T) = Internal.Min.Invoke a b
    static member inline max (a: ^T) (b: ^T) = Internal.Max.Invoke a b
    static member inline sum (collection: ^Collection) = Internal.Sum.Invoke collection
    static member inline average (collection: ^Collection) = Internal.Average.Invoke collection
    static member inline default_value<^t when (^t or DefaultValue) : (static member DefaultValue: ^t -> ^t)>() = Internal.DefaultValue.Invoke<^t>()
    static member inline filter (predicate: 'T -> bool) (collection: 'Collection) = Internal.Filter.Invoke predicate collection
    static member inline choose (chooser: 'T -> 'U option) (collection: 'Collection) = Internal.Choose.Invoke chooser collection
    static member inline find (predicate: 'T -> bool) (collection: 'Collection) = Internal.Find.Invoke predicate collection
    static member inline tryFind (predicate: 'T -> bool) (collection: 'Collection) = Internal.TryFind.Invoke predicate collection
    static member inline equals (a: ^T) (b: ^T) = Internal.Equals.Invoke a b
    static member inline not_equals (a: ^T) (b: ^T) = not (Internal.Equals.Invoke a b)

// Make the public functions available at module level
let inline add a b = Abstract.add a b
let inline subtract a b = Abstract.subtract a b
let inline multiply a b = Abstract.multiply a b
let inline divide a b = Abstract.divide a b
let inline min a b = Abstract.min a b
let inline max a b = Abstract.max a b
let inline sum collection = Abstract.sum collection
let inline average collection = Abstract.average collection
let inline default_value<^t when (^t or DefaultValue) : (static member DefaultValue: ^t -> ^t)>() = Abstract.default_value<^t>()
let inline filter predicate collection = Abstract.filter predicate collection
let inline choose chooser collection = Abstract.choose chooser collection
let inline find predicate collection = Abstract.find predicate collection
let inline tryFind predicate collection = Abstract.tryFind predicate collection
let inline equals a b = Abstract.equals a b
let inline not_equals a b = Abstract.not_equals a b

// Use existing fsil names where appropriate
let inline has_value x = Fsil.Abstract.is_some x
let inline value x = Fsil.Abstract.value x
let inline some x = Fsil.Abstract.some x
let inline some<'a, 'b when 'a: (static member Some: 'b -> 'a)> (value: 'b) : 'a =
    'a.Some value
let inline none() = Fsil.Abstract.none
// Correctly implemented to_string function with proper constraint
let inline to_string<'a when (^a or ^Unit) : (static member ToString: ^a -> string)> (value: 'a) =
    ((^a or ^Unit) : (static member ToString : 'a -> string) value)

let inline print x = Fsil.Abstract.print x