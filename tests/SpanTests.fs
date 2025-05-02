module Alloy.Tests.SpanTests

open System
open Expecto
open Alloy
open Alloy.Span
open Alloy.Tests.TestHelpers

/// Tests for the Span module
let spanTests =
    testList "Span" [
        testList "Span Creation" [
            testCase "asSpan creates Span from array" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asSpan array
                
                Expect.equal span.Length array.Length "Span length should match array length"
                for i = 0 to array.Length - 1 do
                    Expect.equal span.[i] array.[i] $"Span element at {i} should match array element"
            
            testCase "sliceSpan creates Span from array slice" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = sliceSpan array 1 3
                
                Expect.equal span.Length 3 "Sliced span length should match specified length"
                Expect.equal span.[0] 2 "First element of span should be array[start]"
                Expect.equal span.[1] 3 "Second element of span should be array[start+1]"
                Expect.equal span.[2] 4 "Third element of span should be array[start+2]"
            
            testCase "asReadOnlySpan creates ReadOnlySpan from array" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asReadOnlySpan array
                
                Expect.equal span.Length array.Length "ReadOnlySpan length should match array length"
                for i = 0 to array.Length - 1 do
                    Expect.equal span.[i] array.[i] $"ReadOnlySpan element at {i} should match array element"
            
            testCase "sliceReadOnlySpan creates ReadOnlySpan from array slice" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = sliceReadOnlySpan array 1 3
                
                Expect.equal span.Length 3 "Sliced ReadOnlySpan length should match specified length"
                Expect.equal span.[0] 2 "First element of ReadOnlySpan should be array[start]"
                Expect.equal span.[1] 3 "Second element of ReadOnlySpan should be array[start+1]"
                Expect.equal span.[2] 4 "Third element of ReadOnlySpan should be array[start+2]"
        ]
        
        testList "Span Operations" [
            testCase "mapSpan applies function to elements" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate sourceArray.Length
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                mapSpan (fun x -> x * 2) source dest
                
                let expected = [|2; 4; 6; 8; 10|]
                for i = 0 to destArray.Length - 1 do
                    Expect.equal destArray.[i] expected.[i] $"Mapped element at {i} should be double the source"
            
            testCase "mapSpan handles destination shorter than source" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate 3
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                mapSpan (fun x -> x * 2) source dest
                
                let expected = [|2; 4; 6|]
                for i = 0 to destArray.Length - 1 do
                    Expect.equal destArray.[i] expected.[i] $"Mapped element at {i} should be double the source"
            
            testCase "mapSpan handles destination longer than source" <| fun _ ->
                let sourceArray = [|1; 2; 3|]
                let destArray = Array.zeroCreate 5
                destArray.[3] <- 99
                destArray.[4] <- 99
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                mapSpan (fun x -> x * 2) source dest
                
                Expect.equal destArray.[0] 2 "First element should be mapped"
                Expect.equal destArray.[1] 4 "Second element should be mapped"
                Expect.equal destArray.[2] 6 "Third element should be mapped"
                Expect.equal destArray.[3] 99 "Fourth element should remain unchanged"
                Expect.equal destArray.[4] 99 "Fifth element should remain unchanged"
            
            testCase "fillSpan fills span with specified value" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asSpan array
                
                fillSpan 42 span
                
                for i = 0 to array.Length - 1 do
                    Expect.equal array.[i] 42 $"Element at {i} should be filled with 42"
            
            testCase "clearSpan sets elements to default value" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asSpan array
                
                clearSpan span
                
                for i = 0 to array.Length - 1 do
                    Expect.equal array.[i] 0 $"Element at {i} should be cleared to 0"
            
            testCase "copySpan copies elements from source to destination" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate sourceArray.Length
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                copySpan source dest
                
                for i = 0 to destArray.Length - 1 do
                    Expect.equal destArray.[i] sourceArray.[i] $"Copied element at {i} should match source"
            
            testCase "filterSpan selects elements based on predicate" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate sourceArray.Length
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                let count = filterSpan (fun x -> x % 2 = 0) source dest
                
                Expect.equal count 2 "Filter should return count of matched elements"
                Expect.equal destArray.[0] 2 "First element should be 2"
                Expect.equal destArray.[1] 4 "Second element should be 4"
                // Other elements remain as default
            
            testCase "filterSpan handles destination shorter than matching elements" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5; 6; 8; 10|]
                let destArray = Array.zeroCreate 3
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                let count = filterSpan (fun x -> x % 2 = 0) source dest
                
                Expect.equal count 3 "Filter should return count of matched elements that fit"
                Expect.equal destArray.[0] 2 "First element should be 2"
                Expect.equal destArray.[1] 4 "Second element should be 4"
                Expect.equal destArray.[2] 6 "Third element should be 6"
            
            testCase "foldSpan accumulates values correctly" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asReadOnlySpan array
                
                let sum = foldSpan (fun state x -> state + x) 0 span
                
                Expect.equal sum 15 "Fold should sum all elements"
                
                let product = foldSpan (fun state x -> state * x) 1 span
                
                Expect.equal product 120 "Fold should multiply all elements"
            
            testCase "sumSpan computes sum of elements" <| fun _ ->
                let intArray = [|1; 2; 3; 4; 5|]
                let floatArray = [|1.5; 2.5; 3.5; 4.5; 5.5|]
                
                let intSum = sumSpan (asReadOnlySpan intArray)
                let floatSum = sumSpan (asReadOnlySpan floatArray)
                
                Expect.equal intSum 15 "sumSpan should compute correct int sum"
                Expect.equal floatSum 17.5 "sumSpan should compute correct float sum"
            
            testCase "sumSpan handles empty span" <| fun _ ->
                let emptyIntArray: int[] = [||]
                let emptyFloatArray: float[] = [||]
                
                let intSum = sumSpan (asReadOnlySpan emptyIntArray)
                let floatSum = sumSpan (asReadOnlySpan emptyFloatArray)
                
                Expect.equal intSum 0 "sumSpan should return zero for empty int span"
                Expect.equal floatSum 0.0 "sumSpan should return zero for empty float span"
            
            testCase "averageSpan computes average of elements" <| fun _ ->
                let intArray = [|1; 2; 3; 4; 5|]
                let floatArray = [|1.5; 2.5; 3.5; 4.5; 5.5|]
                
                let intAvg = averageSpan (asReadOnlySpan intArray)
                let floatAvg = averageSpan (asReadOnlySpan floatArray)
                
                Expect.equal intAvg 3 "averageSpan should compute correct int average"
                expectFloatClose Accuracy.medium floatAvg 3.5 "averageSpan should compute correct float average"
            
            testCase "averageSpan handles empty span" <| fun _ ->
                let emptyIntArray: int[] = [||]
                let emptyFloatArray: float[] = [||]
                
                let intAvg = averageSpan (asReadOnlySpan emptyIntArray)
                let floatAvg = averageSpan (asReadOnlySpan emptyFloatArray)
                
                Expect.equal intAvg 0 "averageSpan should return zero for empty int span"
                Expect.equal floatAvg 0.0 "averageSpan should return zero for empty float span"
        ]
    ]

// Register the tests
[<Tests>]
let tests = spanTests