module Alloy.Tests.OptionTests

open Expecto
open Alloy

/// Tests for ValueOption type
let optionTests =
    testList "ValueOption" [
        testCase "ValueOption.IsSome - matches Option.IsSome" <| fun _ ->
            let someOpt = Some 42
            let noneOpt = None
            let someValOpt = ValueOption<int>.Some 42
            let noneValOpt = ValueOption<int>.None()
            
            Expect.equal someOpt.IsSome someValOpt.IsSome "IsSome should match for Some values"
            Expect.equal noneOpt.IsSome noneValOpt.IsSome "IsSome should match for None values"
        
        testCase "ValueOption.Value - matches Option.Value" <| fun _ ->
            let someOpt = Some 42
            let someValOpt = ValueOption<int>.Some 42
            
            Expect.equal someOpt.Value someValOpt.Value "Value should match for Some values"
            
            // We can't directly test None.Value as it throws an exception
            // but we can test that both throw
            Expect.throws (fun () -> 
                None.Value |> ignore
            ) "Option.None.Value should throw"
            
            Expect.throws (fun () -> 
                ValueOption<int>.None().Value |> ignore
            ) "ValueOption.None.Value should throw"
        
        testCase "ValueOption.Map - matches Option.map" <| fun _ ->
            let someOpt = Some 42
            let noneOpt = None
            let someValOpt = ValueOption<int>.Some 42
            let noneValOpt = ValueOption<int>.None()
            
            let f = fun x -> x * 2
            
            let mappedOpt = Option.map f someOpt
            let mappedValOpt = ValueOption<int>.Map(f, someValOpt)
            
            Expect.equal mappedOpt.IsSome mappedValOpt.IsSome "IsSome should match after mapping Some"
            Expect.equal mappedOpt.Value mappedValOpt.Value "Value should match after mapping Some"
            
            let mappedNoneOpt = Option.map f noneOpt
            let mappedNoneValOpt = ValueOption<int>.Map(f, noneValOpt)
            
            Expect.equal mappedNoneOpt.IsSome mappedNoneValOpt.IsSome "IsSome should match after mapping None"
        
        testCase "ValueOption.Bind - matches Option.bind" <| fun _ ->
            let someOpt = Some 42
            let noneOpt = None
            let someValOpt = ValueOption<int>.Some 42
            let noneValOpt = ValueOption<int>.None()
            
            let f = fun x -> if x % 2 = 0 then Some (x / 2) else None
            let fVal = fun x -> if x % 2 = 0 then ValueOption<int>.Some (x / 2) else ValueOption<int>.None()
            
            let boundOpt = Option.bind f someOpt
            let boundValOpt = ValueOption<int>.Bind(fVal, someValOpt)
            
            Expect.equal boundOpt.IsSome boundValOpt.IsSome "IsSome should match after binding Some"
            if boundOpt.IsSome then
                Expect.equal boundOpt.Value boundValOpt.Value "Value should match after binding Some"
            
            let boundNoneOpt = Option.bind f noneOpt
            let boundNoneValOpt = ValueOption<int>.Bind(fVal, noneValOpt)
            
            Expect.equal boundNoneOpt.IsSome boundNoneValOpt.IsSome "IsSome should match after binding None"
        
        testCase "ValueOption.OfOption - converts Option to ValueOption" <| fun _ ->
            let someOpt = Some 42
            let noneOpt = None
            
            let someValOpt = ValueOption<int>.OfOption someOpt
            let noneValOpt = ValueOption<int>.OfOption noneOpt
            
            Expect.equal someOpt.IsSome someValOpt.IsSome "IsSome should match after conversion of Some"
            if someOpt.IsSome then
                Expect.equal someOpt.Value someValOpt.Value "Value should match after conversion of Some"
            
            Expect.equal noneOpt.IsSome noneValOpt.IsSome "IsSome should match after conversion of None"
        
        testCase "ValueOption.ToOption - converts ValueOption to Option" <| fun _ ->
            let someValOpt = ValueOption<int>.Some 42
            let noneValOpt = ValueOption<int>.None()
            
            let someOpt = ValueOption<int>.ToOption someValOpt
            let noneOpt = ValueOption<int>.ToOption noneValOpt
            
            Expect.equal someValOpt.IsSome someOpt.IsSome "IsSome should match after conversion of Some"
            if someValOpt.IsSome then
                Expect.equal someValOpt.Value someOpt.Value "Value should match after conversion of Some"
            
            Expect.equal noneValOpt.IsSome noneOpt.IsSome "IsSome should match after conversion of None"
        
        testCase "ValueOption is a struct (zero allocation)" <| fun _ ->
            // Verify that ValueOption is a struct type (zero allocation)
            Expect.isTrue (typeof<ValueOption<int>>.IsValueType) "ValueOption should be a value type"
        
    ]

// Add tests to the test group
[<Tests>]
let tests = optionTests