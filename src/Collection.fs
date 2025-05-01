namespace Alloy

/// Collection operations
module Collection =
    open Core
    
    /// Map operation for collection types
    let map<'T, 'U, 'Collection> (f: 'T -> 'U) (collection: 'Collection) =
        ((^Collection or ^T or ^U) : (static member Map : ('T -> 'U) * 'Collection -> 'U) (f, collection))
    
    /// Map with index
    let mapi<'T, 'U, 'Collection> (f: int -> 'T -> 'U) (collection: 'Collection) =
        ((^Collection or ^T or ^U) : (static member MapI : (int -> 'T -> 'U) * 'Collection -> 'U) (f, collection))
    
    /// Iteration over collection
    let iter<'T, 'Collection> (f: 'T -> unit) (collection: 'Collection) =
        ((^Collection or ^T) : (static member Iter : ('T -> unit) * 'Collection -> unit) (f, collection))
    
    /// Iteration with index
    let iteri<'T, 'Collection> (f: int -> 'T -> unit) (collection: 'Collection) =
        ((^Collection or ^T) : (static member IterI : (int -> 'T -> unit) * 'Collection -> unit) (f, collection))
    
    /// Fold over collection
    let fold<'T, 'State, 'Collection> (folder: 'State -> 'T -> 'State) (state: 'State) (collection: 'Collection) =
        ((^Collection or ^T or ^State) : (static member Fold : ('State -> 'T -> 'State) * 'State * 'Collection -> 'State) 
            (folder, state, collection))
    
    /// Filter collection elements
    let filter<'T, 'Collection> (predicate: 'T -> bool) (collection: 'Collection) =
        ((^Collection or ^T) : (static member Filter : ('T -> bool) * 'Collection -> 'Collection) 
            (predicate, collection))
    
    /// Choose elements with transformation
    let choose<'T, 'U, 'Collection> (chooser: 'T -> 'U option) (collection: 'Collection) =
        ((^Collection or ^T or ^U) : (static member Choose : ('T -> 'U option) * 'Collection -> 'Collection) 
            (chooser, collection))
    
    /// Find an element in a collection
    let find<'T, 'Collection> (predicate: 'T -> bool) (collection: 'Collection) =
        ((^Collection or ^T) : (static member Find : ('T -> bool) * 'Collection -> 'T) 
            (predicate, collection))
    
    /// Try to find an element
    let tryFind<'T, 'Collection> (predicate: 'T -> bool) (collection: 'Collection) =
        ((^Collection or ^T) : (static member TryFind : ('T -> bool) * 'Collection -> 'T option) 
            (predicate, collection))
    
    /// Implementations for Array type
    type ArrayImpl =
        static member Map (f: 'T -> 'U, xs: 'T[]) = Array.map f xs
        static member MapI (f: int -> 'T -> 'U, xs: 'T[]) = Array.mapi f xs
        static member Iter (f: 'T -> unit, xs: 'T[]) = Array.iter f xs
        static member IterI (f: int -> 'T -> unit, xs: 'T[]) = Array.iteri f xs
        static member Fold (folder: 'State -> 'T -> 'State, state: 'State, xs: 'T[]) = 
            Array.fold folder state xs
        static member Filter (predicate: 'T -> bool, xs: 'T[]) = Array.filter predicate xs
        static member Choose (chooser: 'T -> 'U option, xs: 'T[]) = Array.choose chooser xs
        static member Find (predicate: 'T -> bool, xs: 'T[]) = Array.find predicate xs
        static member TryFind (predicate: 'T -> bool, xs: 'T[]) = Array.tryFind predicate xs