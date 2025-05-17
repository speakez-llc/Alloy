module Alloy.Tests.ValueOptionTests

open Expecto
open Alloy

/// Tests for ValueOption<'T> struct type
let valueOptionTests =
    testList "ValueOption" [
        testList "Type Properties" [
            testCase "ValueOption is a value type" <| fun _ ->
                Expect.isTrue (typeof<ValueOption<int>>.IsValueType) "ValueOption should be a value type (struct)"
        ]
        
        testList "Construction" [
            testCase "Static constructors create correct instances" <| fun _ ->
                let someValue = ValueOption.Some 42
                let noneValue = ValueOption<int>.None
                
                Expect.isTrue someValue.IsSome "ValueOption.Some should create a value with IsSome true"
                Expect.equal someValue.Value 42 "ValueOption.Some should store the provided value"
                Expect.isFalse noneValue.IsSome "ValueOption.None should create a value with IsSome false"
                Expect.isTrue noneValue.IsNone "ValueOption.None should create a value with IsNone true"
                Expect.throws (fun () -> noneValue.Value |> ignore) "Accessing Value on None should throw"
            
            testCase "Module functions create correct instances" <| fun _ ->
                let someValue = ValueOption.some 42
                let noneValue = ValueOption.none<int>
                
                Expect.isTrue someValue.IsSome "ValueOption.some should create a value with IsSome true"
                Expect.equal someValue.Value 42 "ValueOption.some should store the provided value"
                Expect.isFalse noneValue.IsSome "ValueOption.none should create a value with IsSome false"
                Expect.isTrue noneValue.IsNone "ValueOption.none should create a value with IsNone true"
            
            testCase "Auto-opened constructors create correct instances" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                Expect.isTrue someValue.IsSome "Some should create a value with IsSome true"
                Expect.equal someValue.Value 42 "Some should store the provided value"
                Expect.isFalse noneValue.IsSome "None should create a value with IsSome false"
                Expect.isTrue noneValue.IsNone "None should create a value with IsNone true"
        ]
        
        testList "Property Access" [
            testCase "IsSome/IsNone properties work correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                Expect.isTrue someValue.IsSome "IsSome should be true for Some values"
                Expect.isFalse someValue.IsNone "IsNone should be false for Some values"
                Expect.isFalse noneValue.IsSome "IsSome should be false for None values"
                Expect.isTrue noneValue.IsNone "IsNone should be true for None values"
            
            testCase "Value property works correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                Expect.equal someValue.Value 42 "Value should return contained value for Some"
                Expect.throws (fun () -> noneValue.Value |> ignore) "Value should throw exception for None"
            
            testCase "ToString produces correct string representation" <| fun _ ->
                let someInt = Some 42
                let someString = Some "test"
                let noneInt = None<int>
                
                Expect.equal (someInt.ToString()) "Some(42)" "ToString should format Some(int) correctly"
                Expect.equal (someString.ToString()) "Some(test)" "ToString should format Some(string) correctly" 
                Expect.equal (noneInt.ToString()) "None" "ToString should format None correctly"
        ]
        
        testList "Module Functions" [
            testCase "Predicate functions work correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                Expect.isTrue (ValueOption.isSome someValue) "isSome returns true for Some values"
                Expect.isFalse (ValueOption.isSome noneValue) "isSome returns false for None values"
                Expect.isFalse (ValueOption.isNone someValue) "isNone returns false for Some values"
                Expect.isTrue (ValueOption.isNone noneValue) "isNone returns true for None values"
            
            testCase "Value access function works correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                Expect.equal (ValueOption.value someValue) 42 "value returns contained value for Some"
                Expect.throws (fun () -> ValueOption.value noneValue |> ignore) "value throws for None"
            
            testCase "defaultValue function works correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                Expect.equal (ValueOption.defaultValue 99 someValue) 42 "defaultValue returns contained value for Some"
                Expect.equal (ValueOption.defaultValue 99 noneValue) 99 "defaultValue returns default for None"
            
            testCase "defaultWith function works correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                let mutable callCount = 0
                let generator () = callCount <- callCount + 1; 99
                
                Expect.equal (ValueOption.defaultWith generator someValue) 42 
                    "defaultWith returns contained value for Some"
                Expect.equal callCount 0 
                    "defaultWith shouldn't call generator for Some"
                
                callCount <- 0
                Expect.equal (ValueOption.defaultWith generator noneValue) 99 
                    "defaultWith returns generator result for None"
                Expect.equal callCount 1 
                    "defaultWith should call generator once for None"
            
            testCase "map function works correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                let mapFunction x = x * 2
                
                let mappedSome = ValueOption.map mapFunction someValue
                let mappedNone = ValueOption.map mapFunction noneValue
                
                Expect.isTrue mappedSome.IsSome "map preserves Some for Some input"
                Expect.equal mappedSome.Value 84 "map applies function to contained value"
                Expect.isTrue mappedNone.IsNone "map preserves None for None input"
            
            testCase "bind function works correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                let bindEven x = 
                    if x % 2 = 0 then Some (x / 2) 
                    else None<int>
                    
                let bindOdd x = 
                    if x % 2 <> 0 then Some (x / 2) 
                    else None<int>
                
                let boundEven = ValueOption.bind bindEven someValue
                let boundOdd = ValueOption.bind bindOdd someValue
                let boundNone = ValueOption.bind bindEven noneValue
                
                Expect.isTrue boundEven.IsSome "bind returns Some when binder returns Some"
                Expect.equal boundEven.Value 21 "bind applies binder function correctly"
                Expect.isTrue boundOdd.IsNone "bind returns None when binder returns None"
                Expect.isTrue boundNone.IsNone "bind preserves None for None input"
        ]
        
        testList "Pattern Matching" [
            testCase "Auto-opened patterns work correctly" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                let matchSome = 
                    match someValue with
                    | Some value -> value * 2
                    | None -> 0
                    
                let matchNone = 
                    match noneValue with
                    | Some value -> value * 2
                    | None -> 0
                
                Expect.equal matchSome 84 "Pattern matching extracts value from Some"
                Expect.equal matchNone 0 "Pattern matching handles None correctly"
            
            testCase "Nested pattern matching works correctly" <| fun _ ->
                let pair = (Some 10, Some 20)
                let pairWithNone = (Some 10, None<int>)
                
                let matchPair p =
                    match p with
                    | (Some a, Some b) -> a + b
                    | (Some a, None) -> a
                    | (None, Some b) -> b
                    | (None, None) -> 0
                
                Expect.equal (matchPair pair) 30 "Pattern matching handles nested Some values"
                Expect.equal (matchPair pairWithNone) 10 "Pattern matching handles mixed Some/None values"
        ]
        
        testList "Complex Scenarios" [
            testCase "Conditional expressions with properties work" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                let result1 = 
                    if someValue.IsSome then someValue.Value * 2
                    else 0
                
                let result2 = 
                    if noneValue.IsSome then noneValue.Value * 2
                    else 0
                
                Expect.equal result1 84 "Conditionals with IsSome/Value work for Some"
                Expect.equal result2 0 "Conditionals with IsSome/Value work for None"
            
            testCase "Compound conditionals work" <| fun _ ->
                let pair = (Some 10, Some 20)
                let pairWithNone = (Some 10, None<int>)
                
                let pairResult = 
                    if fst(pair).IsSome && snd(pair).IsSome then
                        fst(pair).Value + snd(pair).Value
                    else if fst(pair).IsSome then
                        fst(pair).Value
                    else if snd(pair).IsSome then
                        snd(pair).Value
                    else
                        0
                
                let mixedResult = 
                    if fst(pairWithNone).IsSome && snd(pairWithNone).IsSome then
                        fst(pairWithNone).Value + snd(pairWithNone).Value
                    else if fst(pairWithNone).IsSome then
                        fst(pairWithNone).Value
                    else if snd(pairWithNone).IsSome then
                        snd(pairWithNone).Value
                    else
                        0
                
                Expect.equal pairResult 30 "Compound conditionals work with all Some values"
                Expect.equal mixedResult 10 "Compound conditionals work with mixed Some/None values"
            
            testCase "Composing multiple ValueOption operations" <| fun _ ->
                let input = Some 42
                
                let result = 
                    input
                    |> ValueOption.map (fun x -> x * 2)
                    |> ValueOption.bind (fun x -> 
                        if x <= 80 then Some (x / 2)
                        else None)
                    |> ValueOption.defaultValue 0
                
                Expect.equal result 0 "Composed operations respect None in the pipeline"
                
                let input2 = Some 50
                
                let result2 = 
                    input2
                    |> ValueOption.map (fun x -> x * 2)
                    |> ValueOption.bind (fun x -> 
                        if x > 80 then Some (x / 2)
                        else None)
                    |> ValueOption.defaultValue 0
                
                Expect.equal result2 50 "Composed operations calculate correct results"
        ]
        
        testList "Integration with Result" [
            testCase "ofValueOption converts ValueOption to Result" <| fun _ ->
                let someValue = Some 42
                let noneValue = None<int>
                
                let okResult = Result.ofValueOption "empty" someValue
                let errorResult = Result.ofValueOption "empty" noneValue
                
                Expect.equal okResult (Ok 42) "ofValueOption converts Some to Ok"
                Expect.equal errorResult (Error "empty") "ofValueOption converts None to Error with provided message"
            
            testCase "toValueOption converts Result to ValueOption" <| fun _ ->
                let okResult = Ok 42
                let errorResult = Error "error"
                
                let someOption = Result.toValueOption okResult
                let noneOption = Result.toValueOption errorResult
                
                Expect.isTrue someOption.IsSome "toValueOption converts Ok to Some"
                Expect.equal someOption.Value 42 "toValueOption preserves Ok value"
                Expect.isTrue noneOption.IsNone "toValueOption converts Error to None"
        ]
    ]

// Register the tests
[<Tests>]
let tests = valueOptionTests