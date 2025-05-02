module Alloy.Tests.OperatorTests

open Expecto
open Alloy
open Alloy.Core
open Alloy.Numeric
open Alloy.Operators
open Alloy.Tests.TestHelpers

/// Tests for the Operators module
let operatorTests =
    testList "Operators" [
        testList "Function Composition" [
            testCase "Forward pipe operator works correctly" <| fun _ ->
                let add5 x = x + 5
                let result = 10 |> add5
                Expect.equal result 15 "Forward pipe should apply function to value"
                
                // More complex pipes
                let result2 = [|1..5|] |> Array.map (fun x -> x * 2) |> Array.sum
                Expect.equal result2 30 "Forward pipe should work for multi-step operations"
            
            testCase "Backward pipe operator works correctly" <| fun _ ->
                let add5 x = x + 5
                let result = add5 <| 10
                Expect.equal result 15 "Backward pipe should apply function to value"
                
                // With nested expressions
                let result2 = Array.sum <| Array.map (fun x -> x * 2) <| [|1..5|]
                Expect.equal result2 30 "Backward pipe should work for multi-step operations"
            
            testCase "Forward composition operator works correctly" <| fun _ ->
                let add5 x = x + 5
                let multiply2 x = x * 2
                let composed = add5 >> multiply2
                let result = composed 10
                Expect.equal result 30 "Forward composition should apply functions in order (add then multiply)"
                
                // Compose with pipes
                let result2 = 10 |> (add5 >> multiply2)
                Expect.equal result2 30 "Forward composition should work with pipes"
            
            testCase "Backward composition operator works correctly" <| fun _ ->
                let add5 x = x + 5
                let multiply2 x = x * 2
                let composed = multiply2 << add5
                let result = composed 10
                Expect.equal result 30 "Backward composition should apply functions in order (add then multiply)"
                
                // Compose with pipes
                let result2 = 10 |> (multiply2 << add5)
                Expect.equal result2 30 "Backward composition should work with pipes"
        ]
        
        testList "Arithmetic Operators" [
            testCase "Addition operator works with different types" <| fun _ ->
                // Integers
                Expect.equal (5 + 3) 8 "Addition operator should work with integers"
                
                // Floats
                Expect.equal (5.5 + 3.5) 9.0 "Addition operator should work with floats"
                
                // Custom types (Vector2D)
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 3.0; Y = 4.0 }
                let result = v1 + v2
                Expect.equal result.X 4.0 "Addition operator should add X components"
                Expect.equal result.Y 6.0 "Addition operator should add Y components"
            
            testCase "Subtraction operator works with different types" <| fun _ ->
                // Integers
                Expect.equal (10 - 3) 7 "Subtraction operator should work with integers"
                
                // Floats
                Expect.equal (10.5 - 3.5) 7.0 "Subtraction operator should work with floats"
                
                // Custom types (Vector2D)
                let v1 = { X = 5.0; Y = 7.0 }
                let v2 = { X = 3.0; Y = 2.0 }
                let result = v1 - v2
                Expect.equal result.X 2.0 "Subtraction operator should subtract X components"
                Expect.equal result.Y 5.0 "Subtraction operator should subtract Y components"
            
            testCase "Multiplication operator works with different types" <| fun _ ->
                // Integers
                Expect.equal (5 * 3) 15 "Multiplication operator should work with integers"
                
                // Floats
                Expect.equal (5.5 * 2.0) 11.0 "Multiplication operator should work with floats"
                
                // Custom types (Vector2D)
                let v1 = { X = 2.0; Y = 3.0 }
                let v2 = { X = 4.0; Y = 5.0 }
                let result = v1 * v2
                Expect.equal result.X 8.0 "Multiplication operator should multiply X components"
                Expect.equal result.Y 15.0 "Multiplication operator should multiply Y components"
            
            testCase "Division operator works with different types" <| fun _ ->
                // Integers
                Expect.equal (15 / 3) 5 "Division operator should work with integers"
                
                // Floats
                Expect.equal (15.0 / 3.0) 5.0 "Division operator should work with floats"
                
                // Custom types (Vector2D)
                let v1 = { X = 10.0; Y = 15.0 }
                let v2 = { X = 2.0; Y = 3.0 }
                let result = v1 / v2
                Expect.equal result.X 5.0 "Division operator should divide X components"
                Expect.equal result.Y 5.0 "Division operator should divide Y components"
            
            testCase "Operator precedence follows standard rules" <| fun _ ->
                let result1 = 5 + 3 * 2
                Expect.equal result1 11 "Multiplication should have higher precedence than addition"
                
                let result2 = (5 + 3) * 2
                Expect.equal result2 16 "Parentheses should override precedence"
                
                let result3 = 20 / 4 + 3
                Expect.equal result3 8 "Division should have higher precedence than addition"
                
                let result4 = 20 / (4 + 3)
                Expect.equal result4 (20/7) "Parentheses should override precedence"
        ]
        
        testList "Comparison Operators" [
            testCase "Equality operator works with different types" <| fun _ ->
                // Integers
                Expect.isTrue (5 = 5) "Equality operator should return true for equal integers"
                Expect.isFalse (5 = 6) "Equality operator should return false for different integers"
                
                // Floats
                Expect.isTrue (5.5 = 5.5) "Equality operator should return true for equal floats"
                Expect.isFalse (5.5 = 6.5) "Equality operator should return false for different floats"
                
                // Strings
                Expect.isTrue ("hello" = "hello") "Equality operator should return true for equal strings"
                Expect.isFalse ("hello" = "world") "Equality operator should return false for different strings"
                
                // Custom types (Vector2D)
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 1.0; Y = 2.0 }
                let v3 = { X = 3.0; Y = 4.0 }
                
                Expect.isTrue (v1 = v2) "Equality operator should return true for equal vectors"
                Expect.isFalse (v1 = v3) "Equality operator should return false for different vectors"
            
            testCase "Inequality operator works with different types" <| fun _ ->
                // Integers
                Expect.isFalse (5 <> 5) "Inequality operator should return false for equal integers"
                Expect.isTrue (5 <> 6) "Inequality operator should return true for different integers"
                
                // Floats
                Expect.isFalse (5.5 <> 5.5) "Inequality operator should return false for equal floats"
                Expect.isTrue (5.5 <> 6.5) "Inequality operator should return true for different floats"
                
                // Strings
                Expect.isFalse ("hello" <> "hello") "Inequality operator should return false for equal strings"
                Expect.isTrue ("hello" <> "world") "Inequality operator should return true for different strings"
                
                // Custom types (Vector2D)
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 1.0; Y = 2.0 }
                let v3 = { X = 3.0; Y = 4.0 }
                
                Expect.isFalse (v1 <> v2) "Inequality operator should return false for equal vectors"
                Expect.isTrue (v1 <> v3) "Inequality operator should return true for different vectors"
        ]
        
        testList "Mixed Operator Patterns" [
            testCase "Operators can be mixed with functions" <| fun _ ->
                let add5 x = x + 5
                let multiply2 x = x * 2
                
                let result = multiply2 (add5 10)
                Expect.equal result 30 "Function application should work with operators"
                
                let result2 = 10 |> add5 |> multiply2
                Expect.equal result2 30 "Pipe operator should work with functions using operators"
                
                let composed = add5 >> multiply2
                let result3 = composed 10
                Expect.equal result3 30 "Composition should work with functions using operators"
            
            testCase "Operators work with collection operations" <| fun _ ->
                let numbers = [|1; 2; 3; 4; 5|]
                
                // Using map with operators
                let doubled = map (fun x -> x * 2) numbers
                expectArrayEqual doubled [|2; 4; 6; 8; 10|] "map with operator should work"
                
                // Using filter with operators
                let evens = filter (fun x -> x % 2 = 0) numbers
                expectArrayEqual evens [|2; 4|] "filter with operator should work"
                
                // Complex combination
                let result = 
                    numbers 
                    |> filter (fun x -> x % 2 = 0) 
                    |> map (fun x -> x * 2)
                    |> sum
                Expect.equal result 12 "Complex combination of operators and functions should work"
            
            testCase "Operators can be used with custom types in complex expressions" <| fun _ ->
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 3.0; Y = 4.0 }
                let v3 = { X = 5.0; Y = 6.0 }
                
                // Complex vector arithmetic
                let result = v1 + v2 * v3
                let expected = { X = 1.0 + (3.0 * 5.0); Y = 2.0 + (4.0 * 6.0) }
                
                Expect.equal result.X expected.X "Complex vector arithmetic should work for X"
                Expect.equal result.Y expected.Y "Complex vector arithmetic should work for Y"
                
                // With parentheses
                let result2 = (v1 + v2) * v3
                let expected2 = { X = (1.0 + 3.0) * 5.0; Y = (2.0 + 4.0) * 6.0 }
                
                Expect.equal result2.X expected2.X "Vector arithmetic with parentheses should work for X"
                Expect.equal result2.Y expected2.Y "Vector arithmetic with parentheses should work for Y"
            
            testCase "Operator expressions work within functions" <| fun _ ->
                // Define a function using operators
                let vectorMagnitude (v: Vector2D) =
                    sqrt(v.X * v.X + v.Y * v.Y)
                
                let v = { X = 3.0; Y = 4.0 }
                Expect.equal (vectorMagnitude v) 5.0 "Function using operators should calculate correctly"
                
                // More complex function
                let vectorInterpolate (v1: Vector2D) (v2: Vector2D) (t: float) =
                    v1 * { X = 1.0 - t; Y = 1.0 - t } + v2 * { X = t; Y = t }
                
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 5.0; Y = 6.0 }
                
                let interpolated = vectorInterpolate v1 v2 0.5
                Expect.equal interpolated.X 3.0 "Vector interpolation should calculate X correctly"
                Expect.equal interpolated.Y 4.0 "Vector interpolation should calculate Y correctly"
        ]
    ]

// Register the tests
[<Tests>]
let tests = operatorTests