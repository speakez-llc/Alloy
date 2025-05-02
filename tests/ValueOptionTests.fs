module Alloy.Tests.ValueOptionTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for ValueOption<'T> struct type and module functions
let valueOptionTests =
    testList "ValueOption" [
        testList "Basic Properties" [
            testCase "ValueOption is a struct (value type)" <| fun _ ->
                Expect.isTrue (typeof<ValueOption<int>>.IsValueType) "ValueOption should be a value type"
            
            testCase "ValueSome has correct IsSome/IsNone values" <| fun _ ->
                let someOpt = ValueSome 42
                Expect.isTrue someOpt.IsSome "ValueSome.IsSome should be true"
                Expect.isFalse someOpt.IsNone "ValueSome.IsNone should be false"
            
            testCase "ValueNone has correct IsSome/IsNone values" <| fun _ ->
                let noneOpt = ValueNone
                Expect.isFalse noneOpt.IsSome "ValueNone.IsSome should be false"
                Expect.isTrue noneOpt.IsNone "ValueNone.IsNone should be true"
            
            testCase "ValueSome.Value returns contained value" <| fun _ ->
                let someOpt = ValueSome 42
                Expect.equal someOpt.Value 42 "ValueSome.Value should return contained value"
            
            testCase "ValueNone.Value throws exception" <| fun _ ->
                let noneOpt = ValueNone
                Expect.throws (fun () -> noneOpt.Value |> ignore) "ValueNone.Value should throw exception"
        ]
        
        testList "Value Retrieval" [
            testCase "GetValueOrDefault returns value or default" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.equal (someOpt.GetValueOrDefault(99)) 42 "GetValueOrDefault should return value for ValueSome"
                Expect.equal (noneOpt.GetValueOrDefault(99)) 99 "GetValueOrDefault should return default for ValueNone"
            
            testCase "GetValueOrElse returns value or generates value" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                let mutable callCount = 0
                let generator() = callCount <- callCount + 1; 99
                
                Expect.equal (someOpt.GetValueOrElse(generator)) 42 "GetValueOrElse should return value for ValueSome"
                Expect.equal callCount 0 "Generator should not be called for ValueSome"
                
                Expect.equal (noneOpt.GetValueOrElse(generator)) 99 "GetValueOrElse should return generated value for ValueNone"
                Expect.equal callCount 1 "Generator should be called once for ValueNone"
        ]
        
        testList "Static Properties" [
            testCase "Zero returns ValueNone" <| fun _ ->
                let zero = ValueOption<int>.Zero
                Expect.isTrue zero.IsNone "Zero should return ValueNone"
            
            testCase "DefaultValue returns ValueNone" <| fun _ ->
                let defaultValue = ValueOption<int>.DefaultValue
                Expect.isTrue defaultValue.IsNone "DefaultValue should return ValueNone"
            
            testCase "Some creates ValueSome" <| fun _ ->
                let some = ValueOption<int>.Some(42)
                Expect.isTrue some.IsSome "Some should create ValueSome"
                Expect.equal some.Value 42 "Some should wrap the correct value"
            
            testCase "None returns ValueNone" <| fun _ ->
                let none = ValueOption<int>.None
                Expect.isTrue none.IsNone "None should return ValueNone"
            
            testCase "HasValue checks if option has value" <| fun _ ->
                let someOpt = ValueOption<int>.Some(42)
                let noneOpt = ValueOption<int>.None
                
                Expect.isTrue (ValueOption<int>.HasValue someOpt) "HasValue should return true for ValueSome"
                Expect.isFalse (ValueOption<int>.HasValue noneOpt) "HasValue should return false for ValueNone"
        ]
        
        testList "Processing Functions" [
            testCase "Map transforms value correctly" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let mappedSome = ValueOption<int>.Map((fun x -> x * 2), someOpt)
                Expect.isTrue mappedSome.IsSome "Map should preserve ValueSome"
                Expect.equal mappedSome.Value 84 "Map should transform the value correctly"
                
                let mappedNone = ValueOption<int>.Map((fun x -> x * 2), noneOpt)
                Expect.isTrue mappedNone.IsNone "Map should preserve ValueNone"
            
            testCase "MapIndexed transforms value with index" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let mappedSome = ValueOption<int>.MapIndexed((fun i x -> i + x), someOpt)
                Expect.isTrue mappedSome.IsSome "MapIndexed should preserve ValueSome"
                Expect.equal mappedSome.Value 42 "MapIndexed should transform with index 0"
                
                let mappedNone = ValueOption<int>.MapIndexed((fun i x -> i + x), noneOpt)
                Expect.isTrue mappedNone.IsNone "MapIndexed should preserve ValueNone"
            
            testCase "Iterate applies function to value" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                let mutable sum = 0
                
                ValueOption<int>.Iterate((fun x -> sum <- sum + x), someOpt)
                Expect.equal sum 42 "Iterate should apply function to value"
                
                sum <- 0
                ValueOption<int>.Iterate((fun x -> sum <- sum + x), noneOpt)
                Expect.equal sum 0 "Iterate should not call function for ValueNone"
            
            testCase "IterateIndexed applies function with index" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                let mutable result = 0
                
                ValueOption<int>.IterateIndexed((fun i x -> result <- i + x), someOpt)
                Expect.equal result 42 "IterateIndexed should apply function with index 0"
                
                result <- -1
                ValueOption<int>.IterateIndexed((fun i x -> result <- i + x), noneOpt)
                Expect.equal result -1 "IterateIndexed should not call function for ValueNone"
            
            testCase "Bind applies binding function correctly" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
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
                
                Expect.throws (fun () -> ValueOption<int>.Find((fun x -> true), noneOpt) |> ignore)
                    "Find should throw for ValueNone"
            
            testCase "TryFind returns value as ValueOption" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let foundTrue = ValueOption<int>.TryFind((fun x -> x = 42), someOpt)
                Expect.isTrue foundTrue.IsSome "TryFind should return ValueSome when predicate matches"
                Expect.equal foundTrue.Value 42 "TryFind should return correct value"
                
                let foundFalse = ValueOption<int>.TryFind((fun x -> x = 99), someOpt)
                Expect.isTrue foundFalse.IsNone "TryFind should return ValueNone when predicate doesn't match"
                
                let foundNone = ValueOption<int>.TryFind((fun x -> true), noneOpt)
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
        
        testList "ToString representation" [
            testCase "ValueSome has correct representation" <| fun _ ->
                let valueSome = ValueSome 42
                Expect.equal (valueSome.ToString()) "ValueSome(42)" "ValueSome should have correct string representation"
            
            testCase "ValueNone has correct representation" <| fun _ ->
                let valueNone = ValueNone
                Expect.equal (valueNone.ToString()) "ValueNone" "ValueNone should have correct string representation"
        ]
        
        testList "Module Functions" [
            testCase "some creates ValueSome" <| fun _ ->
                let option = ValueOption.some 42
                Expect.isTrue option.IsSome "some should create ValueSome"
                Expect.equal option.Value 42 "some should wrap correct value"
            
            testCase "none creates ValueNone" <| fun _ ->
                let option = ValueOption.none<int>()
                Expect.isTrue option.IsNone "none should create ValueNone"
            
            testCase "isSome checks for ValueSome" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.isTrue (ValueOption.isSome someOpt) "isSome should return true for ValueSome"
                Expect.isFalse (ValueOption.isSome noneOpt) "isSome should return false for ValueNone"
            
            testCase "isNone checks for ValueNone" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.isFalse (ValueOption.isNone someOpt) "isNone should return false for ValueSome"
                Expect.isTrue (ValueOption.isNone noneOpt) "isNone should return true for ValueNone"
            
            testCase "value returns contained value" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.equal (ValueOption.value someOpt) 42 "value should return contained value for ValueSome"
                Expect.throws (fun () -> ValueOption.value noneOpt |> ignore) "value should throw for ValueNone"
            
            testCase "defaultValue returns value or default" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                Expect.equal (ValueOption.defaultValue 99 someOpt) 42 "defaultValue should return value for ValueSome"
                Expect.equal (ValueOption.defaultValue 99 noneOpt) 99 "defaultValue should return default for ValueNone"
            
            testCase "defaultWith returns value or generates" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                let mutable callCount = 0
                let generator() = callCount <- callCount + 1; 99
                
                Expect.equal (ValueOption.defaultWith generator someOpt) 42 "defaultWith should return value for ValueSome"
                Expect.equal callCount 0 "generator should not be called for ValueSome"
                
                Expect.equal (ValueOption.defaultWith generator noneOpt) 99 "defaultWith should return generated value for ValueNone"
                Expect.equal callCount 1 "generator should be called once for ValueNone"
            
            testCase "map transforms value correctly" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let mappedSome = ValueOption.map (fun x -> x * 2) someOpt
                Expect.isTrue mappedSome.IsSome "map should preserve ValueSome"
                Expect.equal mappedSome.Value 84 "map should transform value correctly"
                
                let mappedNone = ValueOption.map (fun x -> x * 2) noneOpt
                Expect.isTrue mappedNone.IsNone "map should preserve ValueNone"
            
            testCase "iter applies function to value" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                let mutable sum = 0
                
                ValueOption.iter (fun x -> sum <- sum + x) someOpt
                Expect.equal sum 42 "iter should apply function to value"
                
                sum <- 0
                ValueOption.iter (fun x -> sum <- sum + x) noneOpt
                Expect.equal sum 0 "iter should not call function for ValueNone"
            
            testCase "bind applies binding function correctly" <| fun _ ->
                let someOpt = ValueSome 42
                let noneOpt = ValueNone
                
                let binder x = if x % 2 = 0 then ValueSome (x / 2) else ValueNone
                
                let boundSome = ValueOption.bind binder someOpt
                Expect.isTrue boundSome.IsSome "bind should return ValueSome for successful binding"
                Expect.equal boundSome.Value 21 "bind should apply binding function correctly"
                
                let boundSomeToNone = ValueOption.bind (fun _ -> ValueNone) someOpt
                Expect.isTrue boundSomeToNone.IsNone "bind should return ValueNone when binding function returns ValueNone"
                
                let boundNone = ValueOption.bind binder noneOpt
                Expect.isTrue boundNone.IsNone "bind should preserve ValueNone"
            
            testCase "ofOption converts F# option to ValueOption" <| fun _ ->
                let someOpt = Some 42
                let noneOpt = None
                
                let valueSome = ValueOption.ofOption someOpt
                Expect.isTrue valueSome.IsSome "ofOption should convert Some to ValueSome"
                Expect.equal valueSome.Value 42 "ofOption should preserve value"
                
                let valueNone = ValueOption.ofOption noneOpt
                Expect.isTrue valueNone.IsNone "ofOption should convert None to ValueNone"
            
            testCase "toOption converts ValueOption to F# option" <| fun _ ->
                let valueSome = ValueSome 42
                let valueNone = ValueNone
                
                let someOpt = ValueOption.toOption valueSome
                Expect.isTrue someOpt.IsSome "toOption should convert ValueSome to Some"
                Expect.equal someOpt.Value 42 "toOption should preserve value"
                
                let noneOpt = ValueOption.toOption valueNone
                Expect.isTrue noneOpt.IsNone "toOption should convert ValueNone to None"
        ]
    ]

// Register the tests
[<Tests>]
let tests = valueOptionTests