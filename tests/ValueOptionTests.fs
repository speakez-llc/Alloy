module Alloy.Tests.ValueOptionTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for ValueOption<'T> struct type and module functions
let valueOptionTests =
    testList "ValueOption" [
        testList "Type Properties" [
            testCase "ValueOption is a struct (value type)" <| fun _ ->
                Expect.isTrue (typeof<ValueOption<int>>.IsValueType) "ValueOption should be a value type"
            
            testCase "ValueOption has correct IsSome/IsNone behavior" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.isTrue someOpt.IsSome "ValueSome.IsSome should be true"
                Expect.isFalse someOpt.IsNone "ValueSome.IsNone should be false"
                
                Expect.isFalse noneOpt.IsSome "ValueNone.IsSome should be false"
                Expect.isTrue noneOpt.IsNone "ValueNone.IsNone should be true"
            
            testCase "Value property extracts the contained value" <| fun _ ->
                let someOpt = ValueSome 42
                Expect.equal someOpt.Value 42 "ValueSome.Value should return contained value"
                
                let noneOpt = ValueNone
                Expect.throws (fun () -> noneOpt.Value |> ignore) "ValueNone.Value should throw exception"
        ]
        
        testList "Value Retrieval Methods" [
            testCase "GetValueOrDefault retrieves value or returns default" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.equal (someOpt.GetValueOrDefault(99)) 42 "GetValueOrDefault should return value for ValueSome"
                Expect.equal (noneOpt.GetValueOrDefault(99)) 99 "GetValueOrDefault should return default for ValueNone"
            
            testCase "GetValueOrElse retrieves value or uses generator" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let mutable callCount = 0
                let generator () = callCount <- callCount + 1; 99
                
                Expect.equal (someOpt.GetValueOrElse(generator)) 42 "GetValueOrElse should return value for ValueSome"
                Expect.equal callCount 0 "Generator should not be called for ValueSome"
                
                callCount <- 0
                Expect.equal (noneOpt.GetValueOrElse(generator)) 99 "GetValueOrElse should return generated value for ValueNone"
                Expect.equal callCount 1 "Generator should be called once for ValueNone"
        ]
        
        testList "Static Members" [
            testCase "Static members provide correct defaults" <| fun _ ->
                let zero = ValueOption<int>.Zero
                Expect.isTrue zero.IsNone "Zero should return ValueNone"
                
                let defaultValue = ValueOption<int>.DefaultValue
                Expect.isTrue defaultValue.IsNone "DefaultValue should return ValueNone"
                
                let none = ValueOption<int>.None
                Expect.isTrue none.IsNone "None should return ValueNone"
                
                let some = ValueOption<int>.Some(42)
                Expect.isTrue some.IsSome "Some should create ValueSome"
                Expect.equal some.Value 42 "Some should wrap the correct value"
                
                Expect.isTrue (ValueOption<int>.HasValue(some)) "HasValue should return true for ValueSome"
                Expect.isFalse (ValueOption<int>.HasValue(none)) "HasValue should return false for ValueNone"
        ]
        
        testList "Processing Functions" [
            testCase "Map transforms values correctly" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let mappedSome = ValueOption<int>.Map((fun x -> x * 2), someOpt)
                Expect.isTrue mappedSome.IsSome "Map should preserve ValueSome"
                Expect.equal mappedSome.Value 84 "Map should transform the value correctly"
                
                let mappedNone = ValueOption<int>.Map((fun x -> x * 2), noneOpt)
                Expect.isTrue mappedNone.IsNone "Map should preserve ValueNone"
            
            testCase "Iterate applies function to value" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let mutable sum = 0
                ValueOption<int>.Iterate((fun x -> sum <- sum + x), someOpt)
                Expect.equal sum 42 "Iterate should apply function to value"
                
                sum <- 0
                ValueOption<int>.Iterate((fun x -> sum <- sum + x), noneOpt)
                Expect.equal sum 0 "Iterate should not call function for ValueNone"
            
            testCase "Bind applies binding function correctly" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                // Binding function that returns Some for even numbers, None for odd
                let binder x = if x % 2 = 0 then ValueSome (x / 2) else ValueNone
                
                let boundSome = ValueOption<int>.Bind(binder, someOpt)
                Expect.isTrue boundSome.IsSome "Bind should return ValueSome for successful binding"
                Expect.equal boundSome.Value 21 "Bind should apply binding function correctly"
                
                let boundSomeToNone = ValueOption<int>.Bind((fun _ -> ValueNone), someOpt)
                Expect.isTrue boundSomeToNone.IsNone "Bind should return ValueNone when binding function returns ValueNone"
                
                let boundNone = ValueOption<int>.Bind(binder, noneOpt)
                Expect.isTrue boundNone.IsNone "Bind should preserve ValueNone"
            
            testCase "Find returns value that matches predicate" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let foundTrue = ValueOption<int>.Find((fun x -> x = 42), someOpt)
                Expect.equal foundTrue 42 "Find should return value when predicate matches"
                
                Expect.throws (fun () -> ValueOption<int>.Find((fun x -> x = 99), someOpt) |> ignore)
                    "Find should throw when predicate doesn't match"
                
                Expect.throws (fun () -> ValueOption<int>.Find((fun _ -> true), noneOpt) |> ignore)
                    "Find should throw for ValueNone"
            
            testCase "TryFind returns value as ValueOption" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let foundTrue = ValueOption<int>.TryFind((fun x -> x = 42), someOpt)
                Expect.isTrue foundTrue.IsSome "TryFind should return ValueSome when predicate matches"
                Expect.equal foundTrue.Value 42 "TryFind should return correct value"
                
                let foundFalse = ValueOption<int>.TryFind((fun x -> x = 99), someOpt)
                Expect.isTrue foundFalse.IsNone "TryFind should return ValueNone when predicate doesn't match"
                
                let foundNone = ValueOption<int>.TryFind((fun _ -> true), noneOpt)
                Expect.isTrue foundNone.IsNone "TryFind should return ValueNone for ValueNone"
        ]
        
        testList "Conversion Functions" [
            testCase "OfOption converts F# option to ValueOption" <| fun _ ->
                let someOpt = Some 42
                let noneOpt = None
                
                let valueSome = ValueOption<int>.OfOption someOpt
                Expect.isTrue valueSome.IsSome "OfOption should convert Some to ValueSome"
                Expect.equal valueSome.Value 42 "OfOption should preserve value"
                
                let valueNone = ValueOption<int>.OfOption noneOpt
                Expect.isTrue valueNone.IsNone "OfOption should convert None to ValueNone"
            
            testCase "ToOption converts ValueOption to F# option" <| fun _ ->
                let valueSome = ValueSome 42
                let valueNone = ValueNone
                
                let someOpt = ValueOption<int>.ToOption valueSome
                Expect.isTrue someOpt.IsSome "ToOption should convert ValueSome to Some"
                Expect.equal someOpt.Value 42 "ToOption should preserve value"
                
                let noneOpt = ValueOption<int>.ToOption valueNone
                Expect.isTrue noneOpt.IsNone "ToOption should convert ValueNone to None"
        ]
        
        testList "Module Functions" [
            testCase "Module functions work as expected" <| fun _ ->
                // Creation functions
                let someFn = ValueOption.some 42
                Expect.isTrue someFn.IsSome "some should create ValueSome"
                Expect.equal someFn.Value 42 "some should wrap correct value"
                
                let noneFn = ValueOption.none<int>()
                Expect.isTrue noneFn.IsNone "none should create ValueNone"
                
                // Check functions
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.isTrue (ValueOption.isSome someOpt) "isSome should return true for ValueSome"
                Expect.isFalse (ValueOption.isSome noneOpt) "isSome should return false for ValueNone"
                
                Expect.isFalse (ValueOption.isNone someOpt) "isNone should return false for ValueSome"
                Expect.isTrue (ValueOption.isNone noneOpt) "isNone should return true for ValueNone"
                
                // Value extraction
                Expect.equal (ValueOption.value someOpt) 42 "value should return contained value for ValueSome"
                Expect.throws (fun () -> ValueOption.value noneOpt |> ignore) "value should throw for ValueNone"
                
                Expect.equal (ValueOption.defaultValue 99 someOpt) 42 "defaultValue should return value for ValueSome"
                Expect.equal (ValueOption.defaultValue 99 noneOpt) 99 "defaultValue should return default for ValueNone"
                
                let mutable callCount = 0
                let generator () = callCount <- callCount + 1; 99
                
                Expect.equal (ValueOption.defaultWith generator someOpt) 42 "defaultWith should return value for ValueSome"
                Expect.equal callCount 0 "generator should not be called for ValueSome"
                
                callCount <- 0
                Expect.equal (ValueOption.defaultWith generator noneOpt) 99 "defaultWith should return generated value for ValueNone"
                Expect.equal callCount 1 "generator should be called once for ValueNone"
                
                // Transform functions
                let mappedSome = ValueOption.map (fun x -> x * 2) someOpt
                Expect.isTrue mappedSome.IsSome "map should preserve ValueSome"
                Expect.equal mappedSome.Value 84 "map should transform value correctly"
                
                let mappedNone = ValueOption.map (fun x -> x * 2) noneOpt
                Expect.isTrue mappedNone.IsNone "map should preserve ValueNone"
                
                let mutable iterCount = 0
                ValueOption.iter (fun x -> iterCount <- iterCount + 1) someOpt
                Expect.equal iterCount 1 "iter should apply function once for ValueSome"
                
                iterCount <- 0
                ValueOption.iter (fun x -> iterCount <- iterCount + 1) noneOpt
                Expect.equal iterCount 0 "iter should not apply function for ValueNone"
                
                // Binding function that returns Some for even numbers, None for odd
                let binder x = if x % 2 = 0 then ValueSome (x / 2) else ValueNone
                
                let boundSome = ValueOption.bind binder someOpt
                Expect.isTrue boundSome.IsSome "bind should return ValueSome for successful binding"
                Expect.equal boundSome.Value 21 "bind should apply binding function correctly"
                
                let boundNone = ValueOption.bind binder noneOpt
                Expect.isTrue boundNone.IsNone "bind should preserve ValueNone"
                
                // Conversion functions
                let optionSome = Some 42
                let optionNone = None
                
                let fromOptionSome = ValueOption.ofOption optionSome
                Expect.isTrue fromOptionSome.IsSome "ofOption should convert Some to ValueSome"
                Expect.equal fromOptionSome.Value 42 "ofOption should preserve value"
                
                let fromOptionNone = ValueOption.ofOption optionNone
                Expect.isTrue fromOptionNone.IsNone "ofOption should convert None to ValueNone"
                
                let toOptionSome = ValueOption.toOption someOpt
                Expect.isTrue toOptionSome.IsSome "toOption should convert ValueSome to Some"
                Expect.equal toOptionSome.Value 42 "toOption should preserve value"
                
                let toOptionNone = ValueOption.toOption noneOpt
                Expect.isTrue toOptionNone.IsNone "toOption should convert ValueNone to None"
        ]
        
        testList "String Representation" [
            testCase "ToString provides correct representation" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.equal (someOpt.ToString()) "ValueSome(42)" "ValueSome.ToString should include the value"
                Expect.equal (noneOpt.ToString()) "ValueNone" "ValueNone.ToString should be 'ValueNone'"
            ]
    ]

// Register the tests
[<Tests>]
let tests = valueOptionTests