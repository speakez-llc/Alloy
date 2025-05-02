module Alloy.Tests.ValueOptionTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for ValueOption<'T> struct type
let valueOptionTests =
    testList "ValueOption" [
        testCase "Basic properties work correctly" <| fun _ ->
            // Type checks
            Expect.isTrue (typeof<ValueOption<int>>.IsValueType) "ValueOption should be a value type"
            
            // ValueSome
            let someValue = ValueSome 42
            Expect.isTrue someValue.IsSome "IsSome should be true for ValueSome"
            Expect.isFalse someValue.IsNone "IsNone should be false for ValueSome"
            Expect.equal someValue.Value 42 "Value should return the wrapped value"
            
            // ValueNone
            let noneValue = ValueNone
            Expect.isFalse noneValue.IsSome "IsSome should be false for ValueNone"
            Expect.isTrue noneValue.IsNone "IsNone should be true for ValueNone"
            Expect.throws (fun () -> noneValue.Value |> ignore) "Value should throw for ValueNone"
            
            // ToString
            Expect.equal someValue.ToString() "ValueSome(42)" "ToString should format ValueSome correctly"
            Expect.equal noneValue.ToString() "ValueNone" "ToString should format ValueNone correctly"
        
        testCase "Module functions work correctly" <| fun _ ->
            // Creation
            let someValue = ValueSome 42
            let noneValue = ValueNone
            
            // Predicates
            Expect.isTrue (ValueOption.isSome someValue) "isSome should be true for ValueSome"
            Expect.isFalse (ValueOption.isSome noneValue) "isSome should be false for ValueNone"
            Expect.isFalse (ValueOption.isNone someValue) "isNone should be false for ValueSome"
            Expect.isTrue (ValueOption.isNone noneValue) "isNone should be true for ValueNone"
            
            // Value access
            Expect.equal (ValueOption.value someValue) 42 "value should return the underlying value"
            Expect.throws (fun () -> ValueOption.value noneValue |> ignore) "value should throw for ValueNone"
            
            Expect.equal (ValueOption.defaultValue 99 someValue) 42 "defaultValue should return value for ValueSome"
            Expect.equal (ValueOption.defaultValue 99 noneValue) 99 "defaultValue should return default for ValueNone"
            
            let mutable callCount = 0
            let generator () = callCount <- callCount + 1; 99
            
            Expect.equal (ValueOption.defaultWith generator someValue) 42 "defaultWith should return value for ValueSome"
            Expect.equal callCount 0 "generator should not be called for ValueSome"
            
            callCount <- 0
            Expect.equal (ValueOption.defaultWith generator noneValue) 99 "defaultWith should return generator result for ValueNone"
            Expect.equal callCount 1 "generator should be called once for ValueNone"
            
            // Transformations
            let mappedSome = ValueOption.map (fun x -> x * 2) someValue
            let mappedNone = ValueOption.map (fun x -> x * 2) noneValue
            
            // Use equality comparison on IsSome and Value rather than direct pattern matching
            Expect.isTrue mappedSome.IsSome "map should create ValueSome for ValueSome input"
            Expect.equal mappedSome.Value 84 "map should transform the value correctly"
            Expect.isTrue mappedNone.IsNone "map should preserve ValueNone"
            
            // Iteration
            let mutable sum = 0
            ValueOption.iter (fun x -> sum <- sum + x) someValue
            Expect.equal sum 42 "iter should apply function to ValueSome"
            
            sum <- 0
            ValueOption.iter (fun x -> sum <- sum + x) noneValue
            Expect.equal sum 0 "iter should not apply function to ValueNone"
            
            // Binding
            let evenToDivideBy2 x = if x % 2 = 0 then ValueSome(x / 2) else ValueNone
            
            let boundEven = ValueOption.bind evenToDivideBy2 someValue
            let boundNone = ValueOption.bind evenToDivideBy2 noneValue
            
            Expect.isTrue boundEven.IsSome "bind should create ValueSome for successful binding"
            Expect.equal boundEven.Value 21 "bind should apply the binding function correctly"
            Expect.isTrue boundNone.IsNone "bind should preserve ValueNone"
            
            let oddToDivideBy2 x = if x % 2 <> 0 then ValueSome(x / 2) else ValueNone
            let boundToNone = ValueOption.bind oddToDivideBy2 someValue
            
            Expect.isTrue boundToNone.IsNone "bind should return ValueNone when binder returns ValueNone"
            
            // Option conversion
            let fromSome = ValueOption.ofOption (Some 42)
            let fromNone = ValueOption.ofOption None
            
            Expect.isTrue fromSome.IsSome "ofOption should convert Some to ValueSome"
            Expect.equal fromSome.Value 42 "ofOption should preserve the value"
            Expect.isTrue fromNone.IsNone "ofOption should convert None to ValueNone"
            
            let toSome = ValueOption.toOption someValue
            let toNone = ValueOption.toOption noneValue
            
            Expect.isTrue toSome.IsSome "toOption should convert ValueSome to Some"
            Expect.equal toSome.Value 42 "toOption should preserve the value"
            Expect.isTrue toNone.IsNone "toOption should convert ValueNone to None"
    ]

// Register the tests
[<Tests>]
let tests = valueOptionTests