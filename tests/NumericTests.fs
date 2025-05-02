module Alloy.Tests.NumericTests

open Expecto
open Alloy
open Alloy.Core
open Alloy.Numerics
open Alloy.Operators
open Alloy.Tests.TestHelpers


let numericTests =
    testList "Numeric" [
        testList "Primitive Operations" [
            // Direct function tests
            testCase "add works on different numeric types" <| fun _ ->
                Expect.equal (add 5 3) 8 "add should work for integers"
                Expect.equal (add 5.5 3.5) 9.0 "add should work for floats"
                Expect.equal (add 5L 3L) 8L "add should work for int64"
                
            testCase "subtract works on different numeric types" <| fun _ ->
                Expect.equal (subtract 10 3) 7 "subtract should work for integers"
                Expect.equal (subtract 10.5 3.5) 7.0 "subtract should work for floats"
                Expect.equal (subtract 10L 3L) 7L "subtract should work for int64"
                
            testCase "multiply works on different numeric types" <| fun _ ->
                Expect.equal (multiply 5 3) 15 "multiply should work for integers"
                Expect.equal (multiply 5.5 2.0) 11.0 "multiply should work for floats"
                Expect.equal (multiply 5L 3L) 15L "multiply should work for int64"
                
            testCase "divide works on different numeric types" <| fun _ ->
                Expect.equal (divide 15 3) 5 "divide should work for integers"
                Expect.equal (divide 15.0 3.0) 5.0 "divide should work for floats"
                Expect.equal (divide 15L 3L) 5L "divide should work for int64"
                
            testCase "min and max work on different numeric types" <| fun _ ->
                // Use Core's min and max
                let int1, int2 = 10, 20
                let float1, float2 = 10.5, 20.5
                
                let intMin = min int1 int2
                Expect.equal intMin 10 "min should return smaller integer"
                
                let floatMin = min float1 float2
                Expect.equal floatMin 10.5 "min should return smaller float"
                
                let intMax = max int1 int2
                Expect.equal intMax 20 "max should return larger integer"
                
                let floatMax = max float1 float2
                Expect.equal floatMax 20.5 "max should return larger float"
                
            testCase "sum works on different numeric arrays" <| fun _ ->
                Expect.equal (sum [|1; 2; 3; 4; 5|]) 15 "sum should add all integers"
                Expect.equal (sum [|1.5; 2.5; 3.5|]) 7.5 "sum should add all floats"
                Expect.equal (sum [|1L; 2L; 3L; 4L|]) 10L "sum should add all int64s"
                
            testCase "average works on different numeric arrays" <| fun _ ->
                Expect.equal (average [|5; 10; 15|]) 10 "average should calculate mean of integers"
                Expect.equal (average [|1.5; 3.5; 5.5|]) 3.5 "average should calculate mean of floats"
                Expect.equal (average [|5L; 10L; 15L|]) 10L "average should calculate mean of int64s"
        ]
        
        testList "Operators" [
            testCase "Arithmetic operators work on integers" <| fun _ ->
                Expect.equal (5 + 3) 8 "Addition operator should work"
                Expect.equal (10 - 3) 7 "Subtraction operator should work"
                Expect.equal (5 * 3) 15 "Multiplication operator should work"
                Expect.equal (15 / 3) 5 "Division operator should work"
                
            testCase "Arithmetic operators work on floats" <| fun _ ->
                Expect.equal (5.5 + 3.5) 9.0 "Addition operator should work for floats"
                Expect.equal (10.5 - 3.5) 7.0 "Subtraction operator should work for floats"
                Expect.equal (5.5 * 2.0) 11.0 "Multiplication operator should work for floats"
                Expect.equal (15.0 / 3.0) 5.0 "Division operator should work for floats"
                
            testCase "Comparison operators work" <| fun _ ->
                Expect.isTrue (5 = 5) "Equality operator should work for equal values"
                Expect.isFalse (5 = 6) "Equality operator should work for unequal values"
                Expect.isTrue (5 <> 6) "Inequality operator should work for unequal values"
                Expect.isFalse (5 <> 5) "Inequality operator should work for equal values"
        ]
        
        testList "Custom Type Operations" [
            testCase "Operations work with Vector2D" <| fun _ ->
                let v1 = { X = 2.0; Y = 3.0 }
                let v2 = { X = 5.0; Y = 7.0 }
                
                let sumResult = add v1 v2
                Expect.equal sumResult.X 7.0 "add should add X components"
                Expect.equal sumResult.Y 10.0 "add should add Y components"
                
                let diffResult = subtract v2 v1
                Expect.equal diffResult.X 3.0 "subtract should subtract X components"
                Expect.equal diffResult.Y 4.0 "subtract should subtract Y components"
                
                let multResult = multiply v1 v2
                Expect.equal multResult.X 10.0 "multiply should multiply X components"
                Expect.equal multResult.Y 21.0 "multiply should multiply Y components"
                
                let divResult = divide v2 v1
                Expect.equal divResult.X 2.5 "divide should divide X components"
                Expect.equal divResult.Y (7.0/3.0) "divide should divide Y components"
                
            testCase "Operators work with Vector2D" <| fun _ ->
                let v1 = { X = 2.0; Y = 3.0 }
                let v2 = { X = 5.0; Y = 7.0 }
                
                let sumResult = v1 + v2
                Expect.equal sumResult.X 7.0 "Addition operator should add X components"
                Expect.equal sumResult.Y 10.0 "Addition operator should add Y components"
                
                let diffResult = v2 - v1
                Expect.equal diffResult.X 3.0 "Subtraction operator should subtract X components"
                Expect.equal diffResult.Y 4.0 "Subtraction operator should subtract Y components"
                
                let multResult = v1 * v2
                Expect.equal multResult.X 10.0 "Multiplication operator should multiply X components"
                Expect.equal multResult.Y 21.0 "Multiplication operator should multiply Y components"
                
                let divResult = v2 / v1
                Expect.equal divResult.X 2.5 "Division operator should divide X components"
                Expect.equal divResult.Y (7.0/3.0) "Division operator should divide Y components"
            
            testCase "Works with TestPerson custom type" <| fun _ ->
                let p1 = { Name = "John"; Age = 25; IsActive = true }
                let p2 = { Name = "Doe"; Age = 30; IsActive = false }
                
                let combined = add p1 p2
                Expect.equal combined.Name "JohnDoe" "TestPerson addition should concatenate names"
                Expect.equal combined.Age 55 "TestPerson addition should add ages"
                Expect.isTrue combined.IsActive "TestPerson addition should OR IsActive"
                
                // Test the + operator too
                let combined2 = p1 + p2
                Expect.equal combined2.Name "JohnDoe" "TestPerson + operator should concatenate names"
                Expect.equal combined2.Age 55 "TestPerson + operator should add ages"
                Expect.isTrue combined2.IsActive "TestPerson + operator should OR IsActive"
        ]
        
        testList "Advanced Usage" [
            testCase "Chaining operators works correctly" <| fun _ ->
                let result = 5 + 3 * 2
                Expect.equal result 11 "Operator precedence should be respected"
                
                let result2 = (5 + 3) * 2
                Expect.equal result2 16 "Parentheses should override precedence"
                
            testCase "Composition with collection operations" <| fun _ ->
                let numbers = [|1..5|]
                
                // Map + sum
                let doubledSum = numbers |> map (fun x -> x * 2) |> sum
                Expect.equal doubledSum 30 "Map + sum composition should work"
                
                // Filter + sum
                let evenSum = numbers |> filter (fun x -> x % 2 = 0) |> sum
                Expect.equal evenSum 6 "Filter + sum composition should work"
                
                // Filter + map + sum
                let filteredDoubledSum = 
                    numbers 
                    |> filter (fun x -> x % 2 = 0) 
                    |> map (fun x -> x * 2) 
                    |> sum
                Expect.equal filteredDoubledSum 12 "Filter + map + sum composition should work"
            
            testCase "Working with zero and one" <| fun _ ->
                let intZero = zero<int>
                Expect.equal intZero 0 "zero<int> should return 0"
                
                let floatZero = zero<float>
                Expect.equal floatZero 0.0 "zero<float> should return 0.0"
                
                let vectorZero = zero<Vector2D>
                Expect.equal vectorZero.X 0.0 "zero<Vector2D> should have X = 0.0"
                Expect.equal vectorZero.Y 0.0 "zero<Vector2D> should have Y = 0.0"
                
                let intOne = one<int>
                Expect.equal intOne 1 "one<int> should return 1"
                
                let floatOne = one<float>
                Expect.equal floatOne 1.0 "one<float> should return 1.0"
                
                let vectorOne = one<Vector2D>
                Expect.equal vectorOne.X 1.0 "one<Vector2D> should have X = 1.0"
                Expect.equal vectorOne.Y 1.0 "one<Vector2D> should have Y = 1.0"
                
            testCase "Edge cases are handled correctly" <| fun _ ->
                // Empty array operations
                Expect.equal (sum ([||] : int[])) 0 "Sum of empty int array should be 0"
                Expect.equal (sum (Array.empty<float>)) 0.0 "Sum of empty float array should be 0.0"
                Expect.equal (average ([||] : int[])) 0 "Average of empty int array should be 0"
                Expect.equal (average (Array.empty<float>)) 0.0 "Average of empty float array should be 0.0"
                
                // Operations with default values
                let intZero = zero<int>
                Expect.equal (add 5 intZero) 5 "Adding zero to int should be identity"
                Expect.equal (multiply 5 intZero) 0 "Multiplying int by zero should be zero"
        ]
    ]

[<Tests>]
let tests = numericTests