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
            testCase "ofValueOption converts ValueSome to Ok" <| fun _ ->
                let input = ValueSome 5
                let result = Result.ofValueOption "none" input
                Expect.equal result (Ok 5) "ofValueOption should convert ValueSome to Ok"
            
            testCase "ofValueOption converts ValueNone to Error" <| fun _ ->
                let input = ValueNone
                let result = Result.ofValueOption "none" input
                Expect.equal result (Error "none") "ofValueOption should convert ValueNone to Error"
            
            testCase "toValueOption converts Ok to ValueSome" <| fun _ ->
                let input = Ok 5
                let result = Result.toValueOption input
                Expect.equal result (ValueSome 5) "toValueOption should convert Ok to ValueSome"
            
            testCase "toValueOption converts Error to ValueNone" <| fun _ ->
                let input = Error "error"
                let result = Result.toValueOption input
                Expect.equal result ValueNone "toValueOption should convert Error to ValueNone"
            
            testCase "ofOption converts Some to Ok" <| fun _ ->
                let input = Some 5
                let result = Result.ofOption "none" input
                Expect.equal result (Ok 5) "ofOption should convert Some to Ok"
            
            testCase "ofOption converts None to Error" <| fun _ ->
                let input = None
                let result = Result.ofOption "none" input
                Expect.equal result (Error "none") "ofOption should convert None to Error"
            
            testCase "toOption converts Ok to Some" <| fun _ ->
                let input = Ok 5
                let result = Result.toOption input
                Expect.equal result (Some 5) "toOption should convert Ok to Some"
            
            testCase "toOption converts Error to None" <| fun _ ->
                let input = Error "error"
                let result = Result.toOption input
                Expect.equal result None "toOption should convert Error to None"
        ]
        
        testList "Check Functions" [
            testCase "isOk returns true for Ok" <| fun _ ->
                let input = Ok 5
                let result = Result.isOk input
                Expect.isTrue result "isOk should return true for Ok"
            
            testCase "isOk returns false for Error" <| fun _ ->
                let input = Error "error"
                let result = Result.isOk input
                Expect.isFalse result "isOk should return false for Error"
            
            testCase "isError returns true for Error" <| fun _ ->
                let input = Error "error"
                let result = Result.isError input
                Expect.isTrue result "isError should return true for Error"
            
            testCase "isError returns false for Ok" <| fun _ ->
                let input = Ok 5
                let result = Result.isError input
                Expect.isFalse result "isError should return false for Ok"
        ]
        
        testList "Advanced Operations" [
            testCase "bimap transforms both Ok and Error" <| fun _ ->
                let okInput: Result<int, string> = Ok 5
                let errorInput: Result<int, string> = Error "error"
                
                let okResult = Result.bimap (fun x -> x * 2) (fun e -> e.ToUpper()) okInput
                let errorResult = Result.bimap (fun x -> x * 2) (fun e -> e.ToUpper()) errorInput
                
                Expect.equal okResult (Ok 10) "bimap should transform Ok value"
                Expect.equal errorResult (Error "ERROR") "bimap should transform Error value"
            
            testCase "filter keeps Ok value that passes predicate" <| fun _ ->
                let input = Ok 10
                let result = Result.filter (fun x -> x > 5) "too small" input
                Expect.equal result (Ok 10) "filter should keep Ok value that passes predicate"
            
            testCase "filter converts Ok to Error when predicate fails" <| fun _ ->
                let input = Ok 3
                let result = Result.filter (fun x -> x > 5) "too small" input
                Expect.equal result (Error "too small") "filter should convert Ok to Error when predicate fails"
            
            testCase "filter preserves Error value" <| fun _ ->
                let input = Error "original error"
                let result = Result.filter (fun x -> x > 5) "too small" input
                Expect.equal result input "filter should preserve Error value"
            
            testCase "map2 combines two Ok results" <| fun _ ->
                let input1: Result<int, string> = Ok 5
                let input2: Result<int, string> = Ok 10
                let result = Result.map2 (fun a b -> a + b) input1 input2
                Expect.equal result (Ok 15) "map2 should combine two Ok results"
            
            testCase "map2 returns first Error on left side" <| fun _ ->
                let input1: Result<int, string> = Error "error 1"
                let input2: Result<int, string> = Ok 10
                let result = Result.map2 (fun a b -> a + b) input1 input2
                Expect.equal result (Error "error 1") "map2 should return first Error on left side"
            
            testCase "map2 returns first Error on right side" <| fun _ ->
                let input1: Result<int, string> = Ok 5
                let input2: Result<int, string> = Error "error 2"
                let result = Result.map2 (fun a b -> a + b) input1 input2
                Expect.equal result (Error "error 2") "map2 should return first Error on right side"
            
            testCase "map2 returns first Error when both inputs are Error" <| fun _ ->
                let input1: Result<int, string> = Error "error 1"
                let input2: Result<int, string> = Error "error 2"
                let result = Result.map2 (fun a b -> a + b) input1 input2
                Expect.equal result (Error "error 1") "map2 should return first Error when both inputs are Error"
        ]
    ]

// Register the tests
[<Tests>]
let tests = resultTests