module Alloy.Tests.NumericTests

open Expecto
open Alloy
open Alloy.Numeric
open Alloy.Tests.TestHelpers

/// Tests for Numeric module functionality
let numericTests =
    testList "Numeric" [
        testList "Primitive Type Operations" [
            testList "Integer Operations" [
                testCase "add combines two integers correctly" <| fun _ ->
                    let a, b = 5, 3
                    Expect.equal (add a b) 8 "add should correctly sum integers"
                    Expect.equal (add -5 10) 5 "add should handle negative numbers"
                    Expect.equal (add 0 5) 5 "add with zero should be identity"
                
                testCase "subtract computes difference correctly" <| fun _ ->
                    let a, b = 10, 3
                    Expect.equal (subtract a b) 7 "subtract should compute correct difference"
                    Expect.equal (subtract 5 10) -5 "subtract should handle negative results"
                    Expect.equal (subtract 5 0) 5 "subtract zero should be identity"
                
                testCase "multiply computes product correctly" <| fun _ ->
                    let a, b = 5, 3
                    Expect.equal (multiply a b) 15 "multiply should compute correct product"
                    Expect.equal (multiply -5 3) -15 "multiply should handle negative numbers"
                    Expect.equal (multiply 5 0) 0 "multiply by zero should be zero"
                    Expect.equal (multiply 5 1) 5 "multiply by one should be identity"
                
                testCase "divide computes quotient correctly" <| fun _ ->
                    let a, b = 15, 3
                    Expect.equal (divide a b) 5 "divide should compute correct quotient"
                    Expect.equal (divide -15 3) -5 "divide should handle negative numbers"
                    Expect.equal (divide 0 5) 0 "zero divided by number should be zero"
                    Expect.equal (divide 5 1) 5 "divide by one should be identity"
                
                testCase "min returns smaller of two integers" <| fun _ ->
                    let a, b = 5, 10
                    Expect.equal (min a b) 5 "min should return smaller number"
                    Expect.equal (min -5 10) -5 "min should handle negative numbers"
                    Expect.equal (min 5 5) 5 "min of equal numbers should return that number"
                
                testCase "max returns larger of two integers" <| fun _ ->
                    let a, b = 5, 10
                    Expect.equal (max a b) 10 "max should return larger number"
                    Expect.equal (max -5 10) 10 "max should handle negative numbers"
                    Expect.equal (max 5 5) 5 "max of equal numbers should return that number"
                
                testCase "sum computes sum of integer array" <| fun _ ->
                    Expect.equal (sum [|1; 2; 3; 4; 5|]) 15 "sum should compute correct sum"
                    Expect.equal (sum [|-5; 5; 10|]) 10 "sum should handle negative numbers"
                    Expect.equal (sum [||]) 0 "sum of empty array should be zero"
                
                testCase "average computes average of integer array" <| fun _ ->
                    Expect.equal (average [|5; 10; 15|]) 10 "average should compute correct average"
                    Expect.equal (average [|-5; 5; 10|]) 3 "average should handle negative numbers"
                    Expect.equal (average [||]) 0 "average of empty array should be zero"
            ]
            
            testList "Float Operations" [
                testCase "add combines two floats correctly" <| fun _ ->
                    expectFloatClose Accuracy.medium (add 5.5 3.5) 9.0 "add should correctly sum floats"
                    expectFloatClose Accuracy.medium (add -5.5 10.5) 5.0 "add should handle negative numbers"
                    expectFloatClose Accuracy.medium (add 0.0 5.5) 5.5 "add with zero should be identity"
                
                testCase "subtract computes difference correctly" <| fun _ ->
                    expectFloatClose Accuracy.medium (subtract 10.5 3.5) 7.0 "subtract should compute correct difference"
                    expectFloatClose Accuracy.medium (subtract 5.5 10.5) -5.0 "subtract should handle negative results"
                    expectFloatClose Accuracy.medium (subtract 5.5 0.0) 5.5 "subtract zero should be identity"
                
                testCase "multiply computes product correctly" <| fun _ ->
                    expectFloatClose Accuracy.medium (multiply 5.5 3.0) 16.5 "multiply should compute correct product"
                    expectFloatClose Accuracy.medium (multiply -5.5 3.0) -16.5 "multiply should handle negative numbers"
                    expectFloatClose Accuracy.medium (multiply 5.5 0.0) 0.0 "multiply by zero should be zero"
                    expectFloatClose Accuracy.medium (multiply 5.5 1.0) 5.5 "multiply by one should be identity"
                
                testCase "divide computes quotient correctly" <| fun _ ->
                    expectFloatClose Accuracy.medium (divide 16.5 3.0) 5.5 "divide should compute correct quotient"
                    expectFloatClose Accuracy.medium (divide -16.5 3.0) -5.5 "divide should handle negative numbers"
                    expectFloatClose Accuracy.medium (divide 0.0 5.5) 0.0 "zero divided by number should be zero"
                    expectFloatClose Accuracy.medium (divide 5.5 1.0) 5.5 "divide by one should be identity"
                
                testCase "min returns smaller of two floats" <| fun _ ->
                    expectFloatClose Accuracy.medium (min 5.5 10.5) 5.5 "min should return smaller number"
                    expectFloatClose Accuracy.medium (min -5.5 10.5) -5.5 "min should handle negative numbers"
                    expectFloatClose Accuracy.medium (min 5.5 5.5) 5.5 "min of equal numbers should return that number"
                
                testCase "max returns larger of two floats" <| fun _ ->
                    expectFloatClose Accuracy.medium (max 5.5 10.5) 10.5 "max should return larger number"
                    expectFloatClose Accuracy.medium (max -5.5 10.5) 10.5 "max should handle negative numbers"
                    expectFloatClose Accuracy.medium (max 5.5 5.5) 5.5 "max of equal numbers should return that number"
                
                testCase "sum computes sum of float array" <| fun _ ->
                    expectFloatClose Accuracy.medium (sum [|1.5; 2.5; 3.5; 4.5; 5.5|]) 17.5 "sum should compute correct sum"
                    expectFloatClose Accuracy.medium (sum [|-5.5; 5.5; 10.5|]) 10.5 "sum should handle negative numbers"
                    expectFloatClose Accuracy.medium (sum [||]) 0.0 "sum of empty array should be zero"
                
                testCase "average computes average of float array" <| fun _ ->
                    expectFloatClose Accuracy.medium (average [|5.5; 10.5; 15.5|]) 10.5 "average should compute correct average"
                    expectFloatClose Accuracy.medium (average [|-5.5; 5.5; 11.0|]) 3.6666666667 "average should handle negative numbers"
                    expectFloatClose Accuracy.medium (average [||]) 0.0 "average of empty array should be zero"
            ]
        ]
        
        testList "Custom Type Operations" [
            testCase "add combines two vectors correctly" <| fun _ ->
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 3.0; Y = 4.0 }
                let result = add v1 v2
                Expect.equal result.X 4.0 "Vector addition should add X components"
                Expect.equal result.Y 6.0 "Vector addition should add Y components"
            
            testCase "subtract computes vector difference correctly" <| fun _ ->
                let v1 = { X = 5.0; Y = 7.0 }
                let v2 = { X = 3.0; Y = 2.0 }
                let result = subtract v1 v2
                Expect.equal result.X 2.0 "Vector subtraction should subtract X components"
                Expect.equal result.Y 5.0 "Vector subtraction should subtract Y components"
            
            testCase "multiply computes vector product correctly" <| fun _ ->
                let v1 = { X = 2.0; Y = 3.0 }
                let v2 = { X = 4.0; Y = 5.0 }
                let result = multiply v1 v2
                Expect.equal result.X 8.0 "Vector multiplication should multiply X components"
                Expect.equal result.Y 15.0 "Vector multiplication should multiply Y components"
            
            testCase "divide computes vector quotient correctly" <| fun _ ->
                let v1 = { X = 10.0; Y = 15.0 }
                let v2 = { X = 2.0; Y = 3.0 }
                let result = divide v1 v2
                Expect.equal result.X 5.0 "Vector division should divide X components"
                Expect.equal result.Y 5.0 "Vector division should divide Y components"
            
            testCase "min returns component-wise minimum of vectors" <| fun _ ->
                let v1 = { X = 1.0; Y = 5.0 }
                let v2 = { X = 3.0; Y = 2.0 }
                let result = min v1 v2
                Expect.equal result.X 1.0 "Vector min should return min of X components"
                Expect.equal result.Y 2.0 "Vector min should return min of Y components"
            
            testCase "max returns component-wise maximum of vectors" <| fun _ ->
                let v1 = { X = 1.0; Y = 5.0 }
                let v2 = { X = 3.0; Y = 2.0 }
                let result = max v1 v2
                Expect.equal result.X 3.0 "Vector max should return max of X components"
                Expect.equal result.Y 5.0 "Vector max should return max of Y components"
            
            // Note: The Vector2D.Sum static method needs to be added to our test helpers
            testCase "sum computes sum of vector array" <| fun _ ->
                let vectors = [|
                    { X = 1.0; Y = 2.0 }
                    { X = 3.0; Y = 4.0 }
                    { X = 5.0; Y = 6.0 }
                |]
                let result = Vector2D.Sum(vectors)
                Expect.equal result.X 9.0 "Vector sum should add all X components"
                Expect.equal result.Y 12.0 "Vector sum should add all Y components"
            
            // Note: The Vector2D.Average static method needs to be added to our test helpers
            testCase "average computes average of vector array" <| fun _ ->
                let vectors = [|
                    { X = 1.0; Y = 2.0 }
                    { X = 3.0; Y = 4.0 }
                    { X = 5.0; Y = 6.0 }
                |]
                let result = Vector2D.Average(vectors)
                Expect.equal result.X 3.0 "Vector average should average all X components"
                Expect.equal result.Y 4.0 "Vector average should average all Y components"
            
            testCase "Handles empty arrays" <| fun _ ->
                let empty: Vector2D[] = [||]
                let sumResult = Vector2D.Sum(empty)
                let avgResult = Vector2D.Average(empty)
                
                Expect.equal sumResult.X 0.0 "Sum of empty vector array should have X = 0"
                Expect.equal sumResult.Y 0.0 "Sum of empty vector array should have Y = 0"
                Expect.equal avgResult.X 0.0 "Average of empty vector array should have X = 0"
                Expect.equal avgResult.Y 0.0 "Average of empty vector array should have Y = 0"
        ]
    ]

// Register the tests
[<Tests>]
let tests = numericTests