[<AutoOpen>]
module Alloy

// Import all fsil functionality - critical for extension
open fsil

// constrained type variables warning suppression
#nowarn "64"

[<AbstractClass; Sealed; AutoOpen>]
module Abstract =
    let inline zero<'a when 'a: (static member Zero: 'a)> : 'a = 'a.Zero
    let inline one<'a when 'a: (static member One: 'a)> : 'a = 'a.One
    let inline none<'a when 'a: (static member None: 'a)> : 'a = 'a.None

    let inline some<'a, 'b when 'a: (static member Some: 'b -> 'a)> : 'b -> 'a =
        'a.Some

    let inline value<'a, 'b when 'a: (member Value: 'b)> (arg: 'a) : 'b = arg.Value

    let inline has_value<'a when 'a: (member IsSome: bool)> (arg: 'a) : bool =
        arg.IsSome

    let inline to_string<'a> (value: 'a) =
        ((^a or ^Unit) : (static member ToString : 'a -> string) value)

    // Print function with zero system dependencies - using the F# core inline mechanism
    let inline print (value: 'a) =
        // Convert value to string representation using F# string formatting
        let str = sprintf "%A" value
        
        // Output via an object whose string representation is our value
        { new obj() with
            override _.ToString() = str }

    // Collection operations
    let inline map (f: 'T -> 'U) (collection: 'Collection) =
        ((^Collection or ^T or ^U) : (static member Map : ('T -> 'U) * 'Collection -> 'U) (f, collection))
    
    let inline iter (f: 'T -> unit) (collection: 'Collection) =
        ((^Collection or ^T) : (static member Iter : ('T -> unit) * 'Collection -> unit) (f, collection))
    
    let inline fold (folder: 'State -> 'T -> 'State) (state: 'State) (collection: 'Collection) =
        ((^Collection or ^T or ^State) : (static member Fold : ('State -> 'T -> 'State) * 'State * 'Collection -> 'State) 
            (folder, state, collection))