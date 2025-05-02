module Alloy.Tests.StaticOptionTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for StaticOption<'T> struct type
let staticOptionTests =
    testList "StaticOption" [
        testCase "Basic properties work correctly" <| fun _ ->
            // Type checks
            Expect.isTrue (typeof<StaticOption<int>>.IsValueType) "StaticOption should be a value type"
            
            // Some variant
            let someValue = StaticOption.Some 42
            Expect.isTrue (someValue.IsSome) "IsSome should be true for Some"
            Expect.isFalse (someValue.IsNone) "IsNone should be false for Some"
            Expect.equal (someValue.Value) 42 "Value should return the wrapped value"
            
            // None variant
            let noneValue = StaticOption<int>.None
            Expect.isFalse (noneValue.IsSome) "IsSome should be false for None"
            Expect.isTrue (noneValue.IsNone) "IsNone should be true for None"
            Expect.throws (fun () -> noneValue.Value |> ignore) "Value should throw for None"
            
            // ToString
            Expect.equal (someValue.ToString()) "Some(42)" "ToString should format Some correctly"
            Expect.equal (noneValue.ToString()) "None" "ToString should format None correctly"
        
        testCase "Module functions work correctly" <| fun _ ->
            // Creation
            let someValue = StaticOption.some 42
            let noneValue = StaticOption.none<int>
            
            // Predicates
            Expect.isTrue (StaticOption.isSome someValue) "isSome should be true for Some"
            Expect.isFalse (StaticOption.isSome noneValue) "isSome should be false for None"
            Expect.isFalse (StaticOption.isNone someValue) "isNone should be false for Some"
            Expect.isTrue (StaticOption.isNone noneValue) "isNone should be true for None"
            
            // Value access
            Expect.equal (StaticOption.value someValue) 42 "value should return the underlying value"
            Expect.throws (fun () -> StaticOption.value noneValue |> ignore) "value should throw for None"
            
            Expect.equal (StaticOption.defaultValue 99 someValue) 42 "defaultValue should return value for Some"
            Expect.equal (StaticOption.defaultValue 99 noneValue) 99 "defaultValue should return default for None"
            
            let mutable callCount = 0
            let generator () = callCount <- callCount + 1; 99
            
            Expect.equal (StaticOption.defaultWith generator someValue) 42 "defaultWith should return value for Some"
            Expect.equal callCount 0 "generator should not be called for Some"
            
            callCount <- 0
            Expect.equal (StaticOption.defaultWith generator noneValue) 99 "defaultWith should return generator result for None"
            Expect.equal callCount 1 "generator should be called once for None"
            
            // Transformations
            let mappedSome = StaticOption.map (fun x -> x * 2) someValue
            let mappedNone = StaticOption.map (fun x -> x * 2) noneValue
            
            Expect.isTrue (mappedSome.IsSome) "map should create Some for Some input"
            Expect.equal (mappedSome.Value) 84 "map should transform the value correctly"
            Expect.isTrue (mappedNone.IsNone) "map should preserve None"
            
            // Conditional checks using properties
            let mutable sum = 0
            if someValue.IsSome then
                sum <- sum + someValue.Value
            Expect.equal sum 42 "Property access should work with Some"
            
            sum <- 0
            if noneValue.IsSome then
                sum <- sum + noneValue.Value
            Expect.equal sum 0 "Property access should work with None"
            
            // Binding
            let evenToDivideBy2 x = if x % 2 = 0 then StaticOption.Some(x / 2) else StaticOption<int>.None
            
            let boundEven = StaticOption.bind evenToDivideBy2 someValue
            let boundNone = StaticOption.bind evenToDivideBy2 noneValue
            
            Expect.isTrue (boundEven.IsSome) "bind should create Some for successful binding"
            Expect.equal (boundEven.Value) 21 "bind should apply the binding function correctly"
            Expect.isTrue (boundNone.IsNone) "bind should preserve None"
            
            let oddToDivideBy2 x = if x % 2 <> 0 then StaticOption.Some(x / 2) else StaticOption<int>.None
            let boundToNone = StaticOption.bind oddToDivideBy2 someValue
            
            Expect.isTrue (boundToNone.IsNone) "bind should return None when binder returns None"
            
            // Regular F# option conversion
            let fromSome = StaticOption.ofOption (Some 42)
            let fromNone = StaticOption.ofOption None
            
            Expect.isTrue (fromSome.IsSome) "ofOption should convert Some to Some"
            Expect.equal (fromSome.Value) 42 "ofOption should preserve the value"
            Expect.isTrue (fromNone.IsNone) "ofOption should convert None to None"
            
            let toSome = StaticOption.toOption someValue
            let toNone = StaticOption.toOption noneValue
            
            Expect.isTrue (toSome.IsSome) "toOption should convert Some to Some"
            Expect.equal (toSome.Value) 42 "toOption should preserve the value"
            Expect.isTrue (toNone.IsNone) "toOption should convert None to None"
        
        testCase "Property access works for complex scenarios" <| fun _ ->
            // Create test values
            let option1 = StaticOption.Some 42
            let option2 = StaticOption<int>.None
            
            // Using property access instead of pattern matching
            let result1 = 
                if option1.IsSome then option1.Value * 2
                else 0
            
            let result2 = 
                if option2.IsSome then option2.Value * 2
                else 0
                
            Expect.equal result1 84 "Property access on Some should work"
            Expect.equal result2 0 "Property access on None should work"
            
            // Test with compound conditions
            let optionPair = (StaticOption.Some 10, StaticOption.Some 20)
            
            let pairResult = 
                if fst(optionPair).IsSome && snd(optionPair).IsSome then
                    fst(optionPair).Value + snd(optionPair).Value
                elif fst(optionPair).IsSome then 
                    fst(optionPair).Value
                elif snd(optionPair).IsSome then
                    snd(optionPair).Value
                else 
                    0
                
            Expect.equal pairResult 30 "Compound conditionals should work with properties"
            
        testCase "Use with Result type" <| fun _ ->
            // Integration with Result module
            let someOption = StaticOption.Some 42
            let noneOption = StaticOption<int>.None
            
            let okResult = Result.ofStaticOption "empty" someOption
            Expect.equal okResult (Ok 42) "ofStaticOption should convert Some to Ok"
            
            let errorResult = Result.ofStaticOption "empty" noneOption
            Expect.equal errorResult (Error "empty") "ofStaticOption should convert None to Error"
            
            let backToSome = Result.toStaticOption okResult
            Expect.isTrue (backToSome.IsSome) "toStaticOption should convert Ok back to Some"
            Expect.equal (backToSome.Value) 42 "toStaticOption should preserve the value"
    ]

// Register the tests
[<Tests>]
let tests = staticOptionTests