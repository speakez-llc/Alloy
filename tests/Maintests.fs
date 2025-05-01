module Alloy.Tests.MainTests

open Expecto
open Alloy

/// Tests for all functionality in Main.fs
let mainTests =
    testList "Main" [
        testList "Basic Values" [
            testCase "Zero - matches default values" <| fun _ ->
                Expect.equal (Abstract.zero<int>()) 0 "Zero for int should be 0"
                Expect.equal (Abstract.zero<float>()) 0.0 "Zero for float should be 0.0"
                Expect.equal (Abstract.zero<string>()) "" "Zero for string should be empty string"
                Expect.equal (Abstract.zero<bool>()) false "Zero for bool should be false"
            
            testCase "One - matches expected values" <| fun _ ->
                Expect.equal (Abstract.one<int>()) 1 "One for int should be 1"
                Expect.equal (Abstract.one<float>()) 1.0 "One for float should be 1.0"
                Expect.equal (Abstract.one<bool>()) true "One for bool should be true"
            
            testCase "DefaultValue - matches default values" <| fun _ ->
                Expect.equal (default_value<int>()) 0 "Default for int should be 0"
                Expect.equal (default_value<float>()) 0.0 "Default for float should be 0.0"
                Expect.equal (default_value<string>()) "" "Default for string should be empty string"
                Expect.equal (default_value<bool>()) false "Default for bool should be false"
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
            
            testCase "ToString and Print" <| fun _ ->
                let i = 42
                let f = 3.14
                let s = "hello"
                let b = true
                
                Expect.equal (to_string i) (i.ToString()) "ToString for int should match .ToString()"
                Expect.equal (to_string f) (f.ToString()) "ToString for float should match .ToString()"
                Expect.equal (to_string s) s "ToString for string should be the string itself"
                Expect.equal (to_string b) (b.ToString()) "ToString for bool should match .ToString()"
            ]
        ]
        
        testList "Numeric Operations" [
            testList "Int32 Operations" [
                testCase "Add - matches System addition" <| fun _ ->
                    let a, b = 5, 7
                    Expect.equal (add a b) (a + b) "Add should match native addition"
                
                testCase "Subtract - matches System subtraction" <| fun _ ->
                    let a, b = 15, 7
                    Expect.equal (subtract a b) (a - b) "Subtract should match native subtraction"
                
                testCase "Multiply - matches System multiplication" <| fun _ ->
                    let a, b = 5, 7
                    Expect.equal (multiply a b) (a * b) "Multiply should match native multiplication"
                
                testCase "Divide - matches System division" <| fun _ ->
                    let a, b = 35, 7
                    Expect.equal (divide a b) (a / b) "Divide should match native division"
                
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
        
        testList "Collection Operations" [
            testCase "Map - matches Array.map" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let f = fun x -> x * 2
                Expect.sequenceEqual (map f arr) (Array.map f arr) "Map should match Array.map"
            
            testCase "MapI - matches Array.mapi" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let f = fun i x -> i + x
                Expect.sequenceEqual (mapi f arr) (Array.mapi f arr) "MapI should match Array.mapi"
            
            testCase "Iter - matches Array.iter" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let mutable sum1 = 0
                let mutable sum2 = 0
                iter (fun x -> sum1 <- sum1 + x) arr
                Array.iter (fun x -> sum2 <- sum2 + x) arr
                Expect.equal sum1 sum2 "Iter should match Array.iter"
            
            testCase "IterI - matches Array.iteri" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let mutable sum1 = 0
                let mutable sum2 = 0
                iteri (fun i x -> sum1 <- sum1 + i + x) arr
                Array.iteri (fun i x -> sum2 <- sum2 + i + x) arr
                Expect.equal sum1 sum2 "IterI should match Array.iteri"
            
            testCase "Fold - matches Array.fold" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let folder = fun state x -> state + x
                let initialState = 0
                Expect.equal (fold folder initialState arr) (Array.fold folder initialState arr) "Fold should match Array.fold"
            
            testCase "Filter - matches Array.filter" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let predicate = fun x -> x % 2 = 0
                Expect.sequenceEqual (filter predicate arr) (Array.filter predicate arr) "Filter should match Array.filter"
            
            testCase "Choose - matches Array.choose" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let chooser = fun x -> if x % 2 = 0 then Some (x * 2) else None
                Expect.sequenceEqual (choose chooser arr) (Array.choose chooser arr) "Choose should match Array.choose"
            
            testCase "Find - matches Array.find" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let predicate = fun x -> x > 3
                Expect.equal (find predicate arr) (Array.find predicate arr) "Find should match Array.find"
            
            testCase "TryFind - matches Array.tryFind" <| fun _ ->
                let arr = [|1; 2; 3; 4; 5|]
                let predicate1 = fun x -> x > 3
                let predicate2 = fun x -> x > 10
                Expect.equal (tryFind predicate1 arr) (Array.tryFind predicate1 arr) "TryFind with match should match Array.tryFind"
                Expect.equal (tryFind predicate2 arr) (Array.tryFind predicate2 arr) "TryFind without match should match Array.tryFind"
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
let tests = mainTests