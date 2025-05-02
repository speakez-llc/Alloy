module Alloy.Tests.ResultTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for Result module functionality
let resultTests =
    testList "Result" [
        testList "Basic Operations" [
            testCase "map transforms Ok value correctly" <| fun _ ->
                let input = Ok 5
                let expected = Ok 10
                let result = Result.map (fun x -> x * 2) input
                Expect.equal result expected "map should transform the Ok value"
            
            testCase "map preserves Error value" <| fun _ ->
                let input = Error "error"
                let result = Result.map (fun x -> x * 2) input
                Expect.equal result input "map should preserve the Error value"
            
            testCase "mapError transforms Error value correctly" <| fun _ ->
                let input = Error "error"
                let expected = Error "ERROR"
                let result = Result.mapError (fun e -> e.ToUpper()) input
                Expect.equal result expected "mapError should transform the Error value"
            
            testCase "mapError preserves Ok value" <| fun _ ->
                let input = Ok 5
                let result = Result.mapError (fun e -> e.ToUpper()) input
                Expect.equal result input "mapError should preserve the Ok value"
            
            testCase "bind transforms Ok value with binding function" <| fun _ ->
                let input = Ok 5
                let expected = Ok 10
                let result = Result.bind (fun x -> Ok (x * 2)) input
                Expect.equal result expected "bind should transform the Ok value with binding function"
            
            testCase "bind preserves Error value" <| fun _ ->
                let input: Result<int, string> = Error "error"
                let result = Result.bind (fun x -> Ok (x * 2)) input
                Expect.equal result input "bind should preserve the Error value"
            
            testCase "bind can produce Error from Ok" <| fun _ ->
                let input = Ok 5
                let expected = Error "computed error"
                let result = Result.bind (fun _ -> Error "computed error") input
                Expect.equal result expected "bind should be able to produce Error from Ok"
        ]
        
        testList "Value Extraction" [
            testCase "defaultValue returns Ok value" <| fun _ ->
                let input = Ok 5
                let result = Result.defaultValue 10 input
                Expect.equal result 5 "defaultValue should return the Ok value"
            
            testCase "defaultValue returns default for Error" <| fun _ ->
                let input = Error "error"
                let result = Result.defaultValue 10 input
                Expect.equal result 10 "defaultValue should return the default value for Error"
            
            testCase "defaultWith returns Ok value" <| fun _ ->
                let input = Ok 5
                let result = Result.defaultWith (fun _ -> 10) input
                Expect.equal result 5 "defaultWith should return the Ok value"
            
            testCase "defaultWith applies function to Error" <| fun _ ->
                let input = Error "error"
                let result = Result.defaultWith (fun e -> e.Length) input
                Expect.equal result 5 "defaultWith should apply the function to the Error value"
                
                // Test with side-effects
                let mutable callCount = 0
                let errorHandler _ = callCount <- callCount + 1; 99
                
                let _ = Result.defaultWith errorHandler (Ok 5)
                Expect.equal callCount 0 "defaultWith should not call handler for Ok"
                
                callCount <- 0
                let _ = Result.defaultWith errorHandler (Error "error")
                Expect.equal callCount 1 "defaultWith should call handler once for Error"
        ]
        
        testList "Creation Functions" [
            testCase "ok creates Ok result" <| fun _ ->
                let result = Result.ok 5
                Expect.equal result (Ok 5) "ok should create an Ok result"
            
            testCase "error creates Error result" <| fun _ ->
                let result = Result.error "error"
                Expect.equal result (Error "error") "error should create an Error result"
        ]
        
        testList "Conversion Functions" [
            testCase "ofValueOption converts ValueOption to Result" <| fun _ ->
                let someInput = ValueSome 5
                let noneInput = ValueNone
                
                let someResult = Result.ofValueOption "none" someInput
                Expect.equal someResult (Ok 5) "ofValueOption should convert ValueSome to Ok"
                
                let noneResult = Result.ofValueOption "none" noneInput
                Expect.equal noneResult (Error "none") "ofValueOption should convert ValueNone to Error with provided error"
            
            testCase "toValueOption converts Result to ValueOption" <| fun _ ->
                let okInput = Ok 5
                let errorInput = Error "error"
                
                let someResult = Result.toValueOption okInput
                Expect.equal someResult (ValueSome 5) "toValueOption should convert Ok to ValueSome"
                
                let noneResult = Result.toValueOption errorInput
                Expect.equal noneResult ValueNone "toValueOption should convert Error to ValueNone"
            
            testCase "ofOption converts Option to Result" <| fun _ ->
                let someInput = Some 5
                let noneInput = None
                
                let someResult = Result.ofOption "none" someInput
                Expect.equal someResult (Ok 5) "ofOption should convert Some to Ok"
                
                let noneResult = Result.ofOption "none" noneInput
                Expect.equal noneResult (Error "none") "ofOption should convert None to Error with provided error"
            
            testCase "toOption converts Result to Option" <| fun _ ->
                let okInput = Ok 5
                let errorInput = Error "error"
                
                let someResult = Result.toOption okInput
                Expect.equal someResult (Some 5) "toOption should convert Ok to Some"
                
                let noneResult = Result.toOption errorInput
                Expect.equal noneResult None "toOption should convert Error to None"
        ]
        
        testList "Check Functions" [
            testCase "isOk/isError detect result type correctly" <| fun _ ->
                let okResult = Ok 5
                let errorResult = Error "error"
                
                Expect.isTrue (Result.isOk okResult) "isOk should be true for Ok"
                Expect.isFalse (Result.isOk errorResult) "isOk should be false for Error"
                
                Expect.isFalse (Result.isError okResult) "isError should be false for Ok"
                Expect.isTrue (Result.isError errorResult) "isError should be true for Error"
        ]
        
        testList "Advanced Operations" [
            testCase "bimap transforms both cases correctly" <| fun _ ->
                let okInput = Ok 5
                let errorInput = Error "error"
                
                let okResult = Result.bimap (fun x -> x * 2) (fun e -> e.ToUpper()) okInput
                Expect.equal okResult (Ok 10) "bimap should transform Ok value"
                
                let errorResult = Result.bimap (fun x -> x * 2) (fun e -> e.ToUpper()) errorInput
                Expect.equal errorResult (Error "ERROR") "bimap should transform Error value"
            
            testCase "filter checks Ok value against predicate" <| fun _ ->
                let okInput = Ok 5
                
                let passes = Result.filter (fun x -> x > 0) "negative" okInput
                Expect.equal passes (Ok 5) "filter should pass when predicate is true"
                
                let fails = Result.filter (fun x -> x > 10) "too small" okInput
                Expect.equal fails (Error "too small") "filter should convert to Error when predicate is false"
                
                let errorInput = Error "original error"
                let errorResult = Result.filter (fun _ -> true) "new error" errorInput
                Expect.equal errorResult errorInput "filter should preserve original Error"
            
            testCase "map2 combines two results" <| fun _ ->
                let ok1 = Ok 5
                let ok2 = Ok 10
                
                let errorInput = Error "error"
                
                let combinedOk = Result.map2 (fun a b -> a + b) ok1 ok2
                Expect.equal combinedOk (Ok 15) "map2 should combine two Ok values"
                
                let errorLeft = Result.map2 (fun a b -> a + b) errorInput ok2
                Expect.equal errorLeft errorInput "map2 should prioritize left Error"
                
                let errorRight = Result.map2 (fun a b -> a + b) ok1 errorInput
                Expect.equal errorRight errorInput "map2 should use right Error if left is Ok"
                
                let errorBoth = Result.map2 (fun a b -> a + b) errorInput errorInput
                Expect.equal errorBoth errorInput "map2 should prioritize left Error when both are Error"
            ]
            
            testCase "Complex error handling flow" <| fun _ ->
                // Define a series of operations that might fail
                let safeDivide x y =
                    if y = 0 then Error "Division by zero"
                    else Ok (x / y)
                
                let safeSqrt x =
                    if x < 0 then Error "Cannot take square root of negative number"
                    else Ok (sqrt x)
                
                // Test successful pipeline
                let successResult =
                    Ok 16.0
                    |> Result.bind safeSqrt
                    |> Result.map (fun x -> x * 2.0)
                    |> Result.bind (fun x -> safeDivide x 2.0)
                
                Expect.equal successResult (Ok 2.0) "Success pipeline should compute correctly"
                
                // Test failure in first step
                let failSqrt =
                    Ok -4.0
                    |> Result.bind safeSqrt
                    |> Result.map (fun x -> x * 2.0)
                    |> Result.bind (fun x -> safeDivide x 2.0)
                
                Expect.equal failSqrt (Error "Cannot take square root of negative number") "First failure should short-circuit"
                
                // Test failure in last step
                let failDivide =
                    Ok 16.0
                    |> Result.bind safeSqrt
                    |> Result.map (fun x -> x * 2.0)
                    |> Result.bind (fun x -> safeDivide x 0.0)
                
                Expect.equal failDivide (Error "Division by zero") "Last failure should be returned"
                
                // Test error mapping
                let errorHandled =
                    failDivide 
                    |> Result.mapError (fun e -> $"Calculation error: {e}")
                    |> Result.defaultValue 0.0
                
                Expect.equal errorHandled 0.0 "Error should be mapped and default used"
    ]

// Register the tests
[<Tests>]
let tests = resultTests