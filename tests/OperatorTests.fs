module Alloy.Tests.OperatorTests

open Expecto
open Alloy
open Alloy.Numeric
open Alloy.Core
open Alloy.Operators

/// Tests for operator overloads
let operatorTests =
    testList "Operators" [
        testCase "Pipe Operators" <| fun _ ->
            let f x = x * 2
            let g x = x + 10
            
            let x = 5
            
            // Test |>
            Expect.equal (x |> f) (f x) "Pipe forward should apply function to value"
            
            // Test <|
            Expect.equal (f <| x) (f x) "Pipe backward should apply function to value"
            
            // Test >>
            let h1 = f >> g
            Expect.equal (h1 x) (g (f x)) "Forward composition should apply functions in order"
            
            // Test <<
            let h2 = g << f
            Expect.equal (h2 x) (g (f x)) "Backward composition should apply functions in order"
        ]
        
        testCase "Arithmetic Operators - Int" <| fun _ ->
            let a, b = 5, 3
            
            // Test +
            Expect.equal (a + b) (add a b) "Custom + should match add function"
            Expect.equal (a + b) (a + b) "Custom + should match system +"
            
            // Test -
            Expect.equal (a - b) (subtract a b) "Custom - should match subtract function"
            Expect.equal (a - b) (a - b) "Custom - should match system -"
            
            // Test *
            Expect.equal (a * b) (multiply a b) "Custom * should match multiply function"
            Expect.equal (a * b) (a * b) "Custom * should match system *"
            
            // Test /
            Expect.equal (a / b) (divide a b) "Custom / should match divide function"
            Expect.equal (a / b) (a / b) "Custom / should match system /"
        ]
        
        testCase "Arithmetic Operators - Float" <| fun _ ->
            let a, b = 5.5, 3.3
            
            // Test +
            Expect.equal (a + b) (add a b) "Custom + should match add function"
            Expect.equal (a + b) (a + b) "Custom + should match system +"
            
            // Test -
            Expect.equal (a - b) (subtract a b) "Custom - should match subtract function"
            Expect.equal (a - b) (a - b) "Custom - should match system -"
            
            // Test *
            Expect.equal (a * b) (multiply a b) "Custom * should match multiply function"
            Expect.equal (a * b) (a * b) "Custom * should match system *"
            
            // Test /
            Expect.equal (a / b) (divide a b) "Custom / should match divide function"
            Expect.equal (a / b) (a / b) "Custom / should match system /"
        ]
        
        testCase "Comparison Operators" <| fun _ ->
            let i1, i2, i3 = 5, 5, 10
            let f1, f2, f3 = 3.14, 3.14, 2.71
            let s1, s2, s3 = "hello", "hello", "world"
            
            // Test = for ints
            Expect.equal (i1 = i2) (equals i1 i2) "Custom = should match equals function"
            Expect.equal (i1 = i3) (equals i1 i3) "Custom = should match equals function"
            
            // Test <> for ints
            Expect.equal (i1 <> i2) (not_equals i1 i2) "Custom <> should match not_equals function"
            Expect.equal (i1 <> i3) (not_equals i1 i3) "Custom <> should match not_equals function"
            
            // Test = for floats
            Expect.equal (f1 = f2) (equals f1 f2) "Custom = should match equals function"
            Expect.equal (f1 = f3) (equals f1 f3) "Custom = should match equals function"
            
            // Test <> for floats
            Expect.equal (f1 <> f2) (not_equals f1 f2) "Custom <> should match not_equals function"
            Expect.equal (f1 <> f3) (not_equals f1 f3) "Custom <> should match not_equals function"
            
            // Test = for strings
            Expect.equal (s1 = s2) (equals s1 s2) "Custom = should match equals function"
            Expect.equal (s1 = s3) (equals s1 s3) "Custom = should match equals function"
            
            // Test <> for strings
            Expect.equal (s1 <> s2) (not_equals s1 s2) "Custom <> should match not_equals function"
            Expect.equal (s1 <> s3) (not_equals s1 s3) "Custom <> should match not_equals function"
        ]
    ]

// Add tests to the test group
[<Tests>]
let tests = operatorTests