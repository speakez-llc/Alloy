namespace Alloy

open System
open System.Collections.Generic

/// <summary>
/// A zero-allocation, value-type option implementation that avoids heap allocations.
/// </summary>
/// <typeparam name="T">The type of the optional value.</typeparam>
[<Struct>]
[<CustomEquality>]
[<NoComparison>]
type ValueOption<'T> =
    private { 
        hasValue: bool
        value: 'T 
    }
    
    /// <summary>Gets whether this option has a value.</summary>
    member this.IsSome = this.hasValue
    
    /// <summary>Gets whether this option has no value.</summary>
    member this.IsNone = not this.hasValue
    
    /// <summary>Gets the value, or throws an exception if there is no value.</summary>
    /// <exception cref="System.Exception">Thrown when the option has no value.</exception>
    member this.Value = if this.hasValue then this.value else failwith "No value"
    
    /// <summary>Creates a ValueOption with the specified value.</summary>
    /// <param name="value">The value to wrap.</param>
    /// <returns>A ValueOption containing the value.</returns>
    static member Some(value: 'T) = { hasValue = true; value = value }
    
    /// <summary>Creates a ValueOption with no value.</summary>
    /// <returns>A ValueOption with no value.</returns>
    static member None = { hasValue = false; value = Unchecked.defaultof<'T> }
    
    // Static members for SRTP support
    static member Zero = ValueOption<'T>.None
    static member DefaultValue = ValueOption<'T>.None
    
    /// <summary>Returns a string representation of the ValueOption.</summary>
    /// <returns>A string representing the option's value or None.</returns>
    override this.ToString() = 
        if this.hasValue then $"Some({this.value})" else "None"
        
    /// <summary>Determines if this ValueOption is equal to another object.</summary>
    /// <param name="obj">The object to compare with.</param>
    /// <returns>True if the objects are equal, false otherwise.</returns>
    override this.Equals(obj) =
        match obj with
        | :? ValueOption<'T> as other ->
            if this.IsNone && other.IsNone then true
            elif this.IsSome && other.IsSome then 
                EqualityComparer<'T>.Default.Equals(this.Value, other.Value)
            else false
        | _ -> false

    /// <summary>Returns a hash code for this ValueOption.</summary>
    /// <returns>A hash code for this ValueOption.</returns>
    override this.GetHashCode() =
        if this.IsNone then 0
        else 
            let valueHash = 
                if isNull (box this.Value) then 0
                else EqualityComparer<'T>.Default.GetHashCode(this.Value)
            hash (1, valueHash)

    /// <summary>Implements the IEquatable interface.</summary>
    interface IEquatable<ValueOption<'T>> with
        /// <summary>Determines if this ValueOption is equal to another ValueOption.</summary>
        /// <param name="other">The ValueOption to compare with.</param>
        /// <returns>True if the ValueOptions are equal, false otherwise.</returns>
        member this.Equals(other) =
            if this.IsNone && other.IsNone then true
            elif this.IsSome && other.IsSome then 
                EqualityComparer<'T>.Default.Equals(this.Value, other.Value)
            else false

    /// <summary>Determines if two ValueOptions are equal.</summary>
    /// <param name="left">The first ValueOption.</param>
    /// <param name="right">The second ValueOption.</param>
    /// <returns>True if the ValueOptions are equal, false otherwise.</returns>
    static member op_Equality (left: ValueOption<'T>, right: ValueOption<'T>) =
        if left.IsNone && right.IsNone then true
        elif left.IsSome && right.IsSome then 
            EqualityComparer<'T>.Default.Equals(left.Value, right.Value)
        else false
    
    /// <summary>Determines if two ValueOptions are not equal.</summary>
    /// <param name="left">The first ValueOption.</param>
    /// <param name="right">The second ValueOption.</param>
    /// <returns>True if the ValueOptions are not equal, false otherwise.</returns>
    static member op_Inequality (left: ValueOption<'T>, right: ValueOption<'T>) =
        not (ValueOption<'T>.op_Equality(left, right))
    
[<AutoOpen>]
module ValueOptionPatterns =
    /// Pattern matching for ValueOption
    let (|Some|None|) (opt: ValueOption<'T>) =
        if opt.IsSome then Some opt.Value else None

[<AutoOpen>]
module ValueOptionConstructors =
    /// Creates a Some value for ValueOption
    let Some value = ValueOption.Some value
    
    /// Creates a None value for ValueOption
    let None<'T> = ValueOption<'T>.None


/// <summary>
/// Functions for working with the ValueOption&lt;'T&gt; type.
/// </summary>
module ValueOption =
  
    /// <summary>Creates a Some value.</summary>
    /// <param name="v">The value to wrap.</param>
    /// <returns>A ValueOption containing the value.</returns>
    let inline some v = ValueOption.Some v
    
    /// <summary>Returns the None value.</summary>
    /// <returns>A ValueOption with no value.</returns>
    let inline none<'T> = ValueOption<'T>.None
    
    /// <summary>Checks if an option has a value.</summary>
    /// <param name="opt">The option to check.</param>
    /// <returns>True if the option has a value, otherwise false.</returns>
    let inline isSome (opt: ValueOption<'T>) = opt.IsSome
    
    /// <summary>Checks if an option has no value.</summary>
    /// <param name="opt">The option to check.</param>
    /// <returns>True if the option has no value, otherwise false.</returns>
    let inline isNone (opt: ValueOption<'T>) = opt.IsNone
    
    /// <summary>Gets the value of a Some option, throws if None.</summary>
    /// <param name="opt">The option from which to get the value.</param>
    /// <returns>The contained value.</returns>
    /// <exception cref="System.Exception">Thrown when the option has no value.</exception>
    let inline value (opt: ValueOption<'T>) = opt.Value
    
    /// <summary>Returns the value if Some, or the default value if None.</summary>
    /// <param name="defaultValue">The default value to use if the option has no value.</param>
    /// <param name="opt">The option to check.</param>
    /// <returns>The option's value or the default value.</returns>
    let inline defaultValue defaultValue (opt: ValueOption<'T>) =
        if opt.IsSome then opt.Value else defaultValue
        
    /// <summary>Returns the value if Some, or calls the generator function if None.</summary>
    /// <param name="generator">The function to call to generate a value if the option has no value.</param>
    /// <param name="opt">The option to check.</param>
    /// <returns>The option's value or the result of calling the generator.</returns>
    let inline defaultWith generator (opt: ValueOption<'T>) =
        if opt.IsSome then opt.Value else generator()
    
    /// <summary>Transforms the value inside an option.</summary>
    /// <param name="mapping">The function to apply to the value.</param>
    /// <param name="opt">The option to transform.</param>
    /// <returns>A new option with the transformed value, or None if the input was None.</returns>
    let inline map mapping (opt: ValueOption<'T>) =
        if opt.IsSome then ValueOption.Some(mapping opt.Value) 
        else ValueOption<_>.None
    
    /// <summary>Transforms an option with a function that returns an option.</summary>
    /// <param name="binder">The function to apply to the value.</param>
    /// <param name="opt">The option to transform.</param>
    /// <returns>The result of applying the binder to the value, or None if the input was None.</returns>
    let inline bind binder (opt: ValueOption<'T>) =
        if opt.IsSome then binder opt.Value
        else ValueOption<_>.None
    
      
    /// <summary>Converts an Internal ValueOption to a standard F# ValueOption.</summary>
    /// <param name="opt">The ValueOption to convert.</param>
    /// <returns>An option with the same value as the input ValueOption.</returns>
    let inline toOption (opt: ValueOption<'T>) =
        if opt.IsSome then Some opt.Value else None