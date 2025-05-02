module Alloy.Tests.TestHelpers

open Expecto
open System

/// Custom Vector2D type for testing numeric operations and abstractions
type Vector2D = 
    { X: float; Y: float }
    
    // Static members for Alloy operations
    static member Add(a: Vector2D, b: Vector2D) = 
        { X = a.X + b.X; Y = a.Y + b.Y }
    
    static member Subtract(a: Vector2D, b: Vector2D) = 
        { X = a.X - b.X; Y = a.Y - b.Y }
    
    static member Multiply(a: Vector2D, b: Vector2D) = 
        { X = a.X * b.X; Y = a.Y * b.Y }
    
    static member Divide(a: Vector2D, b: Vector2D) = 
        { X = a.X / b.X; Y = a.Y / b.Y }
    
    static member Min(a: Vector2D, b: Vector2D) = 
        { X = min a.X b.X; Y = min a.Y b.Y }
    
    static member Max(a: Vector2D, b: Vector2D) = 
        { X = max a.X b.X; Y = max a.Y b.Y }
    
    static member Sum(vectors: Vector2D[]) =
        if vectors.Length = 0 then { X = 0.0; Y = 0.0 }
        else
            vectors |> Array.fold (fun state vec -> 
                { X = state.X + vec.X; Y = state.Y + vec.Y }) { X = 0.0; Y = 0.0 }
    
    static member Average(vectors: Vector2D[]) =
        if vectors.Length = 0 then { X = 0.0; Y = 0.0 }
        else
            let sum = Vector2D.Sum(vectors)
            { X = sum.X / float vectors.Length; Y = sum.Y / float vectors.Length }
    
    static member Zero = { X = 0.0; Y = 0.0 }
    
    static member One = { X = 1.0; Y = 1.0 }
    
    static member DefaultValue = { X = 0.0; Y = 0.0 }
    
    // Match exactly the SRTP constraint signature that default_value function expects
    static member Default (_: Vector2D -> unit) = { X = 0.0; Y = 0.0 }
    
    static member Equals(a: Vector2D, b: Vector2D) =
        abs (a.X - b.X) < 1e-10 && abs (a.Y - b.Y) < 1e-10
        
    override this.ToString() =
        sprintf "{ X = %f; Y = %f }" this.X this.Y

/// Person record for testing custom types
type TestPerson = 
    { Name: string
      Age: int
      IsActive: bool }
    
    static member DefaultValue = { Name = ""; Age = 0; IsActive = false }
    static member Zero = { Name = ""; Age = 0; IsActive = false }
    static member One = { Name = "John"; Age = 1; IsActive = true }
    
    // Match exactly the SRTP constraint signature for default_value
    static member Default (_: TestPerson -> unit) = { Name = ""; Age = 0; IsActive = false }
    
    static member Equals(a: TestPerson, b: TestPerson) =
        a.Name = b.Name && a.Age = b.Age && a.IsActive = b.IsActive
    
    static member Add(a: TestPerson, b: TestPerson) =
        { Name = a.Name + b.Name; Age = a.Age + b.Age; IsActive = a.IsActive || b.IsActive }

/// Test helper functions
let expectEqual<'T when 'T : equality> = Expect.equal

/// Helper for expecting approximate floating-point equality
let expectFloatClose precision (actual: float) (expected: float) message =
    Expect.floatClose precision expected actual message

/// Helper for array equality
let expectArrayEqual<'T when 'T : equality> (actual: 'T[]) (expected: 'T[]) message =
    Expect.sequenceEqual actual expected message

/// Helper for Span equality
let expectSpanEqual<'T when 'T : equality> (actual: Span<'T>) (expected: Span<'T>) message =
    if actual.Length <> expected.Length then
        failtestf "%s. Spans have different lengths: actual=%d, expected=%d" 
            message actual.Length expected.Length
            
    for i = 0 to actual.Length - 1 do
        if not (actual.[i] = expected.[i]) then
            failtestf "%s. Span elements differ at index %d: actual=%A, expected=%A" 
                message i actual.[i] expected.[i]

/// Helper for ReadOnlySpan equality
let expectReadOnlySpanEqual<'T when 'T : equality> 
    (actual: ReadOnlySpan<'T>) (expected: ReadOnlySpan<'T>) message =
    if actual.Length <> expected.Length then
        failtestf "%s. Spans have different lengths: actual=%d, expected=%d" 
            message actual.Length expected.Length
            
    for i = 0 to actual.Length - 1 do
        if not (actual.[i] = expected.[i]) then
            failtestf "%s. Span elements differ at index %d: actual=%A, expected=%A" 
                message i actual.[i] expected.[i]