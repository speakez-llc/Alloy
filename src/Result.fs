namespace Alloy

/// Module containing Result operations
module Result =
    /// Applies a function to a Result value if it's Ok
    let map (f: 'T -> 'U) (result: Result<'T, 'Error>) : Result<'U, 'Error> =
        match result with
        | Ok value -> Ok(f value)
        | Error err -> Error err
    
    /// Applies a function to a Result error if it's Error
    let mapError (f: 'Error1 -> 'Error2) (result: Result<'T, 'Error1>) : Result<'T, 'Error2> =
        match result with
        | Ok value -> Ok value
        | Error err -> Error(f err)
    
    /// Applies a binding function to a Result value if it's Ok
    let bind (f: 'T -> Result<'U, 'Error>) (result: Result<'T, 'Error>) : Result<'U, 'Error> =
        match result with
        | Ok value -> f value
        | Error err -> Error err
    
    /// Returns the result, or a default value if Error
    let defaultValue (defaultValue: 'T) (result: Result<'T, 'Error>) : 'T =
        match result with
        | Ok value -> value
        | Error _ -> defaultValue
    
    /// Returns the result, or applies a function to the error value if Error
    let defaultWith (defaultFactory: 'Error -> 'T) (result: Result<'T, 'Error>) : 'T =
        match result with
        | Ok value -> value
        | Error err -> defaultFactory err
    
    /// Creates an Ok result
    let ok (value: 'T) : Result<'T, 'Error> = Ok value
    
    /// Creates an Error result
    let error (error: 'Error) : Result<'T, 'Error> = Error error
    
    /// Creates a Result from a ValueOption
    /// Returns Ok if Some, or Error with the provided error value if None
    let ofValueOption (error: 'Error) (option: ValueOption<'T>) : Result<'T, 'Error> =
        if option.IsSome then 
            Ok option.Value
        else 
            Error error
    
    /// Creates a ValueOption from a Result
    /// Returns Some if Ok, or None if Error
    let toValueOption (result: Result<'T, 'Error>) : ValueOption<'T> =
        match result with
        | Ok value -> ValueOption.Some value
        | Error _ -> ValueOption<'T>.None
    
    /// Creates a Result from an Option
    /// Returns Ok if Some, or Error with the provided error value if None
    let ofOption (error: 'Error) (option: 'T option) : Result<'T, 'Error> =
        match option with
        | Some value -> Ok value
        | None -> Error error
    
    /// Creates an Option from a Result
    /// Returns Some if Ok, or None if Error
    let toOption (result: Result<'T, 'Error>) : 'T option =
        match result with
        | Ok value -> Some value
        | Error _ -> None
    
    /// Returns true if the result is Ok
    let isOk (result: Result<'T, 'Error>) : bool =
        match result with
        | Ok _ -> true
        | Error _ -> false
    
    /// Returns true if the result is Error
    let isError (result: Result<'T, 'Error>) : bool =
        match result with
        | Ok _ -> false
        | Error _ -> true
    
    /// Converts a Result<'T, 'Error> to a Result<'U, 'Error> by applying a 
    /// function to each case (map for Ok, mapError for Error)
    let bimap (mapFunc: 'T -> 'U) (errorMapFunc: 'Error1 -> 'Error2) (result: Result<'T, 'Error1>) : Result<'U, 'Error2> =
        match result with
        | Ok value -> Ok(mapFunc value)
        | Error err -> Error(errorMapFunc err)
    
    /// Returns the result if Ok and the predicate returns true, otherwise returns the Error case
    let filter (predicate: 'T -> bool) (error: 'Error) (result: Result<'T, 'Error>) : Result<'T, 'Error> =
        match result with
        | Ok value when predicate value -> Ok value
        | Ok _ -> Error error
        | Error err -> Error err
    
    /// Combines two results using a specified function, returns the first Error encountered if either is Error
    let map2 (mapping: 'T1 -> 'T2 -> 'U) (result1: Result<'T1, 'Error>) (result2: Result<'T2, 'Error>) : Result<'U, 'Error> =
        match result1, result2 with
        | Ok value1, Ok value2 -> Ok(mapping value1 value2)
        | Error err, _ -> Error err
        | _, Error err -> Error err