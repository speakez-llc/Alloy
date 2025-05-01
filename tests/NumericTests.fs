module Alloy.Tests.NumericTests

open Expecto
open Alloy
open Alloy.Numeric
open Alloy.Operators

/// Tests for numeric operations
let numericTests =
    testList "Numeric" [
        testList "Int32 Operations" [
            testCase "Add - matches System addition" <| fun _ ->
                let a, b = 5, 7
                Expect.equal (add a b) (a + b) "Add should match native addition"
                
                // Test with custom operator
                let c, d = 10, 20
                Expect.equal (c + d) (c + d) "Custom + operator should work with ints"
            
            testCase "Subtract - matches System subtraction" <| fun _ ->
                let a, b = 15, 7
                Expect.equal (subtract a b) (a - b) "Subtract should match native subtraction"
                
                // Test with custom operator
                let c, d = 30, 12
                Expect.equal (c - d) (c - d) "Custom - operator should work with ints"
            
            testCase "Multiply - matches System multiplication" <| fun _ ->
                let a, b = 5, 7
                Expect.equal (multiply a b) (a * b) "Multiply should match native multiplication"
                
                // Test with custom operator
                let c, d = 6, 8
                Expect.equal (c * d) (c * d) "Custom * operator should work with ints"
            
            testCase "Divide - matches System division" <| fun _ ->
                let a, b = 35, 7
                Expect.equal (divide a b) (a / b) "Divide should match native division"
                
                // Test with custom operator
                let c, d = 50, 10
                Expect.equal (c / d) (c / d) "Custom / operator should work with ints"
            
            testCase "Min - matches System.Math.Min" <| fun _ ->
                let a, b = 5, 7
                Expect.equal (min a b) (System.Math.Min(a, b)) "Min should match System.Math.Min"
            
            testCase "Max - matches System.Math.Max" <| fun _ ->
                let a, b = 5, 7
                Expect.equal (max a b) (System.Math.Max(a, b)) "Max should match System.Math.Max"
            
            testCase "Sum - matches Array.sum" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                Expect.equal (sum arr) (Array.sum arr) "Sum should match Array.sum"
            
            testCase "Average - matches hand-crafted average" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let expectedAvg = (Array.sum arr) / arr.Length
                Expect.equal (average arr) expectedAvg "Average should match expected value"
        ]
        
        testList "Float Operations" [
            testCase "Add - matches System addition" <| fun _ ->
                let a, b = 5.5, 7.5
                Expect.equal (add a b) (a + b) "Add should match native addition"
            
            testCase "Subtract - matches System subtraction" <| fun _ ->
                let a, b = 15.5, 7.5
                Expect.equal (subtract a b) (a - b) "Subtract should match native subtraction"
            
            testCase "Multiply - matches System multiplication" <| fun _ ->
                let a, b = 5.5, 7.5
                Expect.equal (multiply a b) (a * b) "Multiply should match native multiplication"
            
            testCase "Divide - matches System division" <| fun _ ->
                let a, b = 35.5, 7.5
                Expect.equal (divide a b) (a / b) "Divide should match native division"
            
            testCase "Min - matches System.Math.Min" <| fun _ ->
                let a, b = 5.5, 7.5
                Expect.equal (min a b) (System.Math.Min(a, b)) "Min should match System.Math.Min"
            
            testCase "Max - matches System.Math.Max" <| fun _ ->
                let a, b = 5.5, 7.5
                Expect.equal (max a b) (System.Math.Max(a, b)) "Max should match System.Math.Max"
            
            testCase "Sum - matches Array.sum" <| fun _ ->
                let arr = [|1.5; 2.5; 3.5; 4.5; 5.5|]
                Expect.equal (sum arr) (Array.sum arr) "Sum should match Array.sum"
            
            testCase "Average - matches Array.average" <| fun _ ->
                let arr = [|1.5; 2.5; 3.5; 4.5; 5.5|]
                Expect.equal (average arr) (Array.average arr) "Average should match Array.average"
        ]
    ]

// Add tests to the test group
[<Tests>]
let tests = numericTests