module Alloy.Tests.SpanTests

open System
open Expecto
open Alloy
open Alloy.Span
open Alloy.Tests.TestHelpers

/// Tests for Span module
let spanTests =
    testList "Span" [
        testList "Creation Functions" [
            testCase "asSpan creates Span from array" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asSpan array
                
                Expect.equal span.Length array.Length "Span length should match array length"
                for i = 0 to array.Length - 1 do
                    Expect.equal span.[i] array.[i] $"Span[{i}] should match array[{i}]"
            
            testCase "sliceSpan creates Span from array slice" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = sliceSpan array 1 3
                
                Expect.equal span.Length 3 "Span length should match specified length"
                Expect.equal span.[0] array.[1] "Span[0] should match array[start]"
                Expect.equal span.[1] array.[2] "Span[1] should match array[start+1]"
                Expect.equal span.[2] array.[3] "Span[2] should match array[start+2]"
            
            testCase "asReadOnlySpan creates ReadOnlySpan from array" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asReadOnlySpan array
                
                Expect.equal span.Length array.Length "ReadOnlySpan length should match array length"
                for i = 0 to array.Length - 1 do
                    Expect.equal span.[i] array.[i] $"ReadOnlySpan[{i}] should match array[{i}]"
            
            testCase "sliceReadOnlySpan creates ReadOnlySpan from array slice" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = sliceReadOnlySpan array 1 3
                
                Expect.equal span.Length 3 "ReadOnlySpan length should match specified length"
                Expect.equal span.[0] array.[1] "ReadOnlySpan[0] should match array[start]"
                Expect.equal span.[1] array.[2] "ReadOnlySpan[1] should match array[start+1]"
                Expect.equal span.[2] array.[3] "ReadOnlySpan[2] should match array[start+2]"
        ]
        
        testList "Non-Allocating Operations" [
            testCase "mapSpan transforms elements without allocation" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate sourceArray.Length
                
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                mapSpan (fun x -> x * 2) source dest
                
                for i = 0 to sourceArray.Length - 1 do
                    Expect.equal destArray.[i] (sourceArray.[i] * 2) $"dest[{i}] should be source[{i}] * 2"
            
            testCase "mapSpan handles destination shorter than source" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate 3  // Shorter destination
                
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                mapSpan (fun x -> x * 2) source dest
                
                // Should only map elements that fit
                for i = 0 to destArray.Length - 1 do
                    Expect.equal destArray.[i] (sourceArray.[i] * 2) $"dest[{i}] should be source[{i}] * 2"
            
            testCase "fillSpan sets all elements to specified value" <| fun _ ->
                let array = Array.zeroCreate 5
                let span = asSpan array
                
                fillSpan 42 span
                
                for i = 0 to array.Length - 1 do
                    Expect.equal array.[i] 42 $"array[{i}] should be 42"
            
            testCase "clearSpan resets all elements to default" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asSpan array
                
                clearSpan span
                
                for i = 0 to array.Length - 1 do
                    Expect.equal array.[i] 0 $"array[{i}] should be 0"
            
            testCase "copySpan copies elements without allocation" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate sourceArray.Length
                
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                copySpan source dest
                
                for i = 0 to sourceArray.Length - 1 do
                    Expect.equal destArray.[i] sourceArray.[i] $"dest[{i}] should be source[{i}]"
            
            testCase "filterSpan selects elements without allocation" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5|]
                let destArray = Array.zeroCreate sourceArray.Length
                
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                let count = filterSpan (fun x -> x % 2 = 0) source dest
                
                Expect.equal count 2 "filterSpan should return count of matched elements"
                Expect.equal destArray.[0] 2 "First even number is 2"
                Expect.equal destArray.[1] 4 "Second even number is 4"
                
                // The rest should remain as default values
                for i = 2 to destArray.Length - 1 do
                    Expect.equal destArray.[i] 0 $"dest[{i}] should be 0"
            
            testCase "filterSpan handles destination shorter than matches" <| fun _ ->
                let sourceArray = [|1; 2; 3; 4; 5; 6; 8; 10|]
                let destArray = Array.zeroCreate 3  // Only room for 3 matches
                
                let source = asReadOnlySpan sourceArray
                let dest = asSpan destArray
                
                let count = filterSpan (fun x -> x % 2 = 0) source dest
                
                Expect.equal count 3 "filterSpan should only write up to destination capacity"
                Expect.equal destArray.[0] 2 "First even number is 2"
                Expect.equal destArray.[1] 4 "Second even number is 4"
                Expect.equal destArray.[2] 6 "Third even number is 6"
            
            testCase "foldSpan accumulates values correctly" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let span = asReadOnlySpan array
                
                // Sum using fold
                let sum = foldSpan (fun acc x -> acc + x) 0 span
                Expect.equal sum 15 "Sum should be 15"
                
                // Product using fold
                let product = foldSpan (fun acc x -> acc * x) 1 span
                Expect.equal product 120 "Product should be 120"
                
                // More complex fold (sum of squares)
                let sumOfSquares = foldSpan (fun acc x -> acc + x * x) 0 span
                Expect.equal sumOfSquares 55 "Sum of squares should be 55"
        ]
        
        testList "Aggregation Operations" [
            testCase "sumSpan computes sum correctly" <| fun _ ->
                let intArray = [|1; 2; 3; 4; 5|]
                let intSpan = asReadOnlySpan intArray
                
                let intSum = sumSpan intSpan
                Expect.equal intSum 15 "Sum of ints should be 15"
                
                let floatArray = [|1.5; 2.5; 3.5; 4.5; 5.5|]
                let floatSpan = asReadOnlySpan floatArray
                
                let floatSum = sumSpan floatSpan
                Expect.equal floatSum 17.5 "Sum of floats should be 17.5"
            
            testCase "sumSpan handles empty spans" <| fun _ ->
                let emptyIntArray: int[] = [||]
                let emptyIntSpan = asReadOnlySpan emptyIntArray
                
                let emptyIntSum = sumSpan emptyIntSpan
                Expect.equal emptyIntSum 0 "Sum of empty int span should be 0"
                
                let emptyFloatArray: float[] = [||]
                let emptyFloatSpan = asReadOnlySpan emptyFloatArray
                
                let emptyFloatSum = sumSpan emptyFloatSpan
                Expect.equal emptyFloatSum 0.0 "Sum of empty float span should be 0.0"
            
            testCase "averageSpan computes average correctly" <| fun _ ->
                let intArray = [|2; 4; 6; 8; 10|]
                let intSpan = asReadOnlySpan intArray
                
                let intAvg = averageSpan intSpan
                Expect.equal intAvg 6 "Average of ints should be 6"
                
                let floatArray = [|1.5; 2.5; 3.5; 4.5; 5.5|]
                let floatSpan = asReadOnlySpan floatArray
                
                let floatAvg = averageSpan floatSpan
                Expect.equal floatAvg 3.5 "Average of floats should be 3.5"
            
            testCase "averageSpan handles empty spans" <| fun _ ->
                let emptyIntArray: int[] = [||]
                let emptyIntSpan = asReadOnlySpan emptyIntArray
                
                let emptyIntAvg = averageSpan emptyIntSpan
                Expect.equal emptyIntAvg 0 "Average of empty int span should be 0"
                
                let emptyFloatArray: float[] = [||]
                let emptyFloatSpan = asReadOnlySpan emptyFloatArray
                
                let emptyFloatAvg = averageSpan emptyFloatSpan
                Expect.equal emptyFloatAvg 0.0 "Average of empty float span should be 0.0"
        ]
        
        testList "Practical Scenarios" [
            testCase "Efficient number parsing without allocation" <| fun _ ->
                // Example: Parse a string of numbers without creating intermediate arrays
                let numberString = "123,456,789,42,0"
                let expectedNumbers = [|123; 456; 789; 42; 0|]
                
                // Count commas to determine number of items
                let mutable commaCount = 0
                for i = 0 to numberString.Length - 1 do
                    if numberString.[i] = ',' then commaCount <- commaCount + 1
                
                // Create result array with exact size
                let resultArray = Array.zeroCreate (commaCount + 1)
                
                // Parse without allocations
                let mutable startIdx = 0
                let mutable resultIdx = 0
                
                for i = 0 to numberString.Length - 1 do
                    if numberString.[i] = ',' || i = numberString.Length - 1 then
                        let endIdx = if i = numberString.Length - 1 then i else i - 1
                        let len = endIdx - startIdx + 1
                        
                        // Parse the number segment
                        let mutable num = 0
                        for j = startIdx to endIdx do
                            let digit = int numberString.[j] - int '0'
                            num <- num * 10 + digit
                        
                        // Store the result
                        resultArray.[resultIdx] <- num
                        resultIdx <- resultIdx + 1
                        startIdx <- i + 1
                
                // Verify result
                expectArrayEqual resultArray expectedNumbers "Number parsing should extract correct values"
            
            testCase "In-place array transformation" <| fun _ ->
                // Example: Transform an array in-place
                let array = [|1; 2; 3; 4; 5|]
                let mutable span = asSpan array
                
                // Square each element without allocating a new array
                for i = 0 to span.Length - 1 do
                    span.[i] <- span.[i] * span.[i]
                
                let expected = [|1; 4; 9; 16; 25|]
                expectArrayEqual array expected "In-place transformation should square each element"
            
            testCase "Filtering with exact allocation" <| fun _ ->
                // Example: Extract even numbers with exact allocation size
                let sourceArray = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
                
                // First count matches to allocate exact size
                let mutable evenCount = 0
                for i = 0 to sourceArray.Length - 1 do
                    if sourceArray.[i] % 2 = 0 then
                        evenCount <- evenCount + 1
                
                // Allocate result array with exact size
                let resultArray = Array.zeroCreate evenCount
                
                // Use filterSpan to populate result
                let source = asReadOnlySpan sourceArray
                let dest = asSpan resultArray
                
                let count = filterSpan (fun x -> x % 2 = 0) source dest
                
                Expect.equal count evenCount "filterSpan should fill the exact array"
                expectArrayEqual resultArray [|2; 4; 6; 8; 10|] "Result should contain all even numbers"
        ]
    ]


[<Tests>]
let tests = spanTests