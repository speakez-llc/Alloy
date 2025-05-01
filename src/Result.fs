namespace Alloy

/// Result operations
module Result =
    /// Bind operation for monadic composition
    let bind<'T, 'U, 'Error> (f: 'T -> Result<'U, 'Error>) (result: Result<'T, 'Error>) =
        match result with
        | Ok value -> f value
        | Error err -> Error err
    
    /// Map operation for Results
    let map<'T, 'U, 'Error> (f: 'T -> 'U) (result: Result<'T, 'Error>) =
        match result with
        | Ok value -> Ok (f value)
        | Error err -> Error err
    
    /// Map error in Result
    let mapError<'T, 'Error1, 'Error2> (f: 'Error1 -> 'Error2) (result: Result<'T, 'Error1>) =
        match result with
        | Ok value -> Ok value
        | Error err -> Error (f err)
    
    /// Default value if Result is Error
    let defaultValue<'T, 'Error> (defaultValue: 'T) (result: Result<'T, 'Error>) =
        match result with
        | Ok value -> value
        | Error _ -> defaultValue
    
    /// Default value with function if Result is Error
    let defaultWith<'T, 'Error> (defaultFn: 'Error -> 'T) (result: Result<'T, 'Error>) =
        match result with
        | Ok value -> value
        | Error err -> defaultFn err