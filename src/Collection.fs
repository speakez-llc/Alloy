namespace Alloy

/// Collection operations
[<AutoOpen>]
module Collection =
    open Core
    
    /// Map operation for collection types
    let inline map<'T, 'U, 'Collection when 'Collection: (static member Map: ('T -> 'U) * 'Collection -> 'Collection)> 
        (f: 'T -> 'U) (collection: 'Collection) : 'Collection =
        'Collection.Map(f, collection)
    
    /// Map with index
    let inline mapi<'T, 'U, 'Collection when 'Collection: (static member MapI: (int -> 'T -> 'U) * 'Collection -> 'Collection)> 
        (f: int -> 'T -> 'U) (collection: 'Collection) : 'Collection =
        'Collection.MapI(f, collection)
    
    /// Iteration over collection
    let inline iter<'T, 'Collection when 'Collection: (static member Iter: ('T -> unit) * 'Collection -> unit)> 
        (f: 'T -> unit) (collection: 'Collection) : unit =
        'Collection.Iter(f, collection)
    
    /// Iteration with index
    let inline iteri<'T, 'Collection when 'Collection: (static member IterI: (int -> 'T -> unit) * 'Collection -> unit)> 
        (f: int -> 'T -> unit) (collection: 'Collection) : unit =
        'Collection.IterI(f, collection)
    
    /// Fold over collection
    let inline fold<'T, 'State, 'Collection when 'Collection: (static member Fold: ('State -> 'T -> 'State) * 'State * 'Collection -> 'State)> 
        (folder: 'State -> 'T -> 'State) (state: 'State) (collection: 'Collection) : 'State =
        'Collection.Fold(folder, state, collection)
    
    /// Filter collection elements
    let inline filter<'T, 'Collection when 'Collection: (static member Filter: ('T -> bool) * 'Collection -> 'Collection)> 
        (predicate: 'T -> bool) (collection: 'Collection) : 'Collection =
        'Collection.Filter(predicate, collection)
    
    /// Choose elements with transformation
    let inline choose<'T, 'U, 'Collection when 'Collection: (static member Choose: ('T -> 'U option) * 'Collection -> 'Collection)> 
        (chooser: 'T -> 'U option) (collection: 'Collection) : 'Collection =
        'Collection.Choose(chooser, collection)
    
    /// Find an element in a collection
    let inline find<'T, 'Collection when 'Collection: (static member Find: ('T -> bool) * 'Collection -> 'T)> 
        (predicate: 'T -> bool) (collection: 'Collection) : 'T =
        'Collection.Find(predicate, collection)
    
    /// Try to find an element
    let inline tryFind<'T, 'Collection when 'Collection: (static member TryFind: ('T -> bool) * 'Collection -> 'T option)> 
        (predicate: 'T -> bool) (collection: 'Collection) : 'T option =
        'Collection.TryFind(predicate, collection)
    
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