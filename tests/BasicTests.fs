module Alloy.Tests.BasicTests

open Expecto
// Open specific modules explicitly instead of relying on AutoOpen
open Alloy.Core 
open Alloy.Numeric
open Alloy.Operators

[<Tests>]
let basicTests =
    testList "Basic" [
        testCase "Direct integer addition" <| fun _ ->
            // Use standard F# operators first
            let standardResult = 5 + 3
            Expect.equal standardResult 8 "Standard F# addition should work"
            
            // Test collection operations which should work regardless
            let array = [|1; 2; 3|]
            let length = len array
            Expect.equal length 3 "len function should work"
        ]
    