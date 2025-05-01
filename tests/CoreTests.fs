module Alloy.Tests.CoreTests

open Expecto
open Alloy
open Alloy.Core

/// Tests for core operations
let coreTests =
    testList "Core" [
        testList "Basic Values" [
            testCase "Zero - matches default values" <| fun _ ->
                Expect.equal (zero<int>) 0 "Zero for int should be 0"
                Expect.equal (zero<float>) 0.0 "Zero for float should be 0.0"
                Expect.equal (zero<string>) "" "Zero for string should be empty string"
                Expect.equal (zero<bool>) false "Zero for bool should be false"
            
            testCase "One - matches expected values" <| fun _ ->
                Expect.equal (one<int>) 1 "One for int should be 1"
                Expect.equal (one<float>) 1.0 "One for float should be 1.0"
                Expect.equal (one<bool>) true "One for bool should be true"
            
            testCase "DefaultValue - matches default values" <| fun _ ->
                Expect.equal (default_value<int>) 0 "Default for int should be 0"
                Expect.equal (default_value<float>) 0.0 "Default for float should be 0.0"
                Expect.equal (default_value<string>) "" "Default for string should be empty string"
                Expect.equal (default_value<bool>) false "Default for bool should be false"
        ]
        
        testList "Option Operations" [
            testCase "HasValue - matches option.IsSome" <| fun _ ->
                let some = Some 42
                let none = None
                Expect.equal (has_value some) some.IsSome "HasValue for Some should match IsSome"
                Expect.equal (has_value none) none.IsSome "HasValue for None should match IsSome"
            
            testCase "Value - matches option.Value" <| fun _ ->
                let some = Some 42
                Expect.equal (value some) some.Value "Value should match option.Value"
                
                // We can't test None.Value as it throws an exception
            
            testCase "Some - creates Some value" <| fun _ ->
                let x = 42
                let result = some<int, int option> x
                Expect.equal result (Some x) "Some should create Some value"
            
            testCase "None - creates None value" <| fun _ ->
                let result = none<int option> ()
                Expect.equal result None "None should create None value"
        ]
        
        testList "ToString and Print" [
            testCase "ToString - matches .ToString()" <| fun _ ->
                let i = 42
                let f = 3.14
                let s = "hello"
                let b = true
                
                Expect.equal (to_string i) (i.ToString()) "ToString for int should match .ToString()"
                Expect.equal (to_string f) (f.ToString()) "ToString for float should match .ToString()"
                Expect.equal (to_string s) s "ToString for string should be the string itself"
                Expect.equal (to_string b) (b.ToString()) "ToString for bool should match .ToString()"
            
            testCase "Print - returns object with correct ToString" <| fun _ ->
                let x = 42
                let result = print x
                Expect.equal (result.ToString()) (sprintf "%A" x) "Print should return object with correct ToString"
            ]
            
        testList "Equality Operations" [
            testCase "Equals - matches system equality" <| fun _ ->
                let i1, i2, i3 = 42, 42, 43
                let f1, f2, f3 = 3.14, 3.14, 2.71
                let s1, s2, s3 = "hello", "hello", "world"
                let b1, b2, b3 = true, true, false
                
                Expect.equal (equals i1 i2) (i1 = i2) "Equals for equal ints should match =="
                Expect.equal (equals i1 i3) (i1 = i3) "Equals for unequal ints should match =="
                
                Expect.equal (equals f1 f2) (f1 = f2) "Equals for equal floats should match =="
                Expect.equal (equals f1 f3) (f1 = f3) "Equals for unequal floats should match =="
                
                Expect.equal (equals s1 s2) (s1 = s2) "Equals for equal strings should match =="
                Expect.equal (equals s1 s3) (s1 = s3) "Equals for unequal strings should match =="
                
                Expect.equal (equals b1 b2) (b1 = b2) "Equals for equal bools should match =="
                Expect.equal (equals b1 b3) (b1 = b3) "Equals for unequal bools should match =="
            
            testCase "NotEquals - matches system inequality" <| fun _ ->
                let i1, i2, i3 = 42, 42, 43
                let f1, f2, f3 = 3.14, 3.14, 2.71
                let s1, s2, s3 = "hello", "hello", "world"
                let b1, b2, b3 = true, true, false
                
                Expect.equal (not_equals i1 i2) (i1 <> i2) "NotEquals for equal ints should match !="
                Expect.equal (not_equals i1 i3) (i1 <> i3) "NotEquals for unequal ints should match !="
                
                Expect.equal (not_equals f1 f2) (f1 <> f2) "NotEquals for equal floats should match !="
                Expect.equal (not_equals f1 f3) (f1 <> f3) "NotEquals for unequal floats should match !="
                
                Expect.equal (not_equals s1 s2) (s1 <> s2) "NotEquals for equal strings should match !="
                Expect.equal (not_equals s1 s3) (s1 <> s3) "NotEquals for unequal strings should match !="
                
                Expect.equal (not_equals b1 b2) (b1 <> b2) "NotEquals for equal bools should match !="
                Expect.equal (not_equals b1 b3) (b1 <> b3) "NotEquals for unequal bools should match !="
            ]
    ]

// Add tests to the test group
[<Tests>]
let tests = coreTests