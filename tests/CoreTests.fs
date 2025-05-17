module Alloy.Tests.CoreTests

open Expecto
open Alloy
open Alloy.Core
open Alloy.Tests.TestHelpers

/// Tests for Core module functionality
let coreTests =
    testList "Core" [
        testList "Collection Operations" [
            testCase "map transforms collections correctly" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                // Explicitly qualify the map function for arrays
                let result = Core.map (fun x -> x * 2) array
                expectArrayEqual result [|2; 4; 6; 8; 10|] "map should double each element"
                
                // Test with a different transformation
                let result2 = Core.map (fun x -> x * x) array
                expectArrayEqual result2 [|1; 4; 9; 16; 25|] "map should square each element"
            
            testCase "mapi transforms with index correctly" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let result = mapi (fun i x -> x + i) array
                expectArrayEqual result [|1; 3; 5; 7; 9|] "mapi should add index to each element"
                
                // Test with a different transformation
                let result2 = mapi (fun i x -> i * x) array
                expectArrayEqual result2 [|0; 2; 6; 12; 20|] "mapi should multiply element by index"
            
            testCase "iter applies function to all elements" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let mutable sum = 0
                iter (fun x -> sum <- sum + x) array
                Expect.equal sum 15 "iter should add all elements"
                
                // Reset and test with a different operation
                sum <- 0
                iter (fun x -> sum <- sum + x * x) array
                Expect.equal sum 55 "iter should add squares of all elements"
            
            testCase "iteri applies indexed function to all elements" <| fun _ ->
                let array = [|1; 2; 3; 4; 5|]
                let mutable sum = 0
                iteri (fun i x -> sum <- sum + i * x) array
                Expect.equal sum 40 "iteri should add product of index and element"
            
            testCase "filter selects elements correctly" <| fun _ ->
                let array = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
                
                // Filter even numbers
                let evens = filter (fun x -> x % 2 = 0) array
                expectArrayEqual evens [|2; 4; 6; 8; 10|] "filter should select even numbers"
                
                // Filter by more complex predicate
                let divisibleBy3 = filter (fun x -> x % 3 = 0) array
                expectArrayEqual divisibleBy3 [|3; 6; 9|] "filter should select numbers divisible by 3"
                
                // Test with an always-false predicate
                let none = filter (fun _ -> false) array
                expectArrayEqual none [||] "filter with false predicate should return empty array"
                
                // Test with an always-true predicate
                let all = filter (fun _ -> true) array
                expectArrayEqual all array "filter with true predicate should return all elements"
            
            testCase "choose applies chooser function correctly" <| fun _ ->
                let array = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
                
                // Using standard F# options for the choose function since that's what Core.choose expects
                let result = choose (fun x -> if x % 2 = 0 then Microsoft.FSharp.Core.Some (x * 2) else Microsoft.FSharp.Core.None) array
                expectArrayEqual result [|4; 8; 12; 16; 20|] "choose should transform filtered elements"
                
                // Choose with more complex logic - using standard F# options
                let complexChooser x = 
                    match x with
                    | 1 -> Microsoft.FSharp.Core.Some 2     // x+1
                    | 3 -> Microsoft.FSharp.Core.Some 9     // x*x
                    | 4 -> Microsoft.FSharp.Core.Some 5     // x+1
                    | 6 -> Microsoft.FSharp.Core.Some 36    // x*x
                    | 7 -> Microsoft.FSharp.Core.Some 8     // x+1
                    | 9 -> Microsoft.FSharp.Core.Some 81    // x*x
                    | _ -> Microsoft.FSharp.Core.None
                    
                let result2 = choose complexChooser array
                expectArrayEqual result2 [|2; 9; 5; 36; 8; 81|] "choose should apply complex transformation"
            
            testCase "len returns correct collection length" <| fun _ ->
                Expect.equal (len [|1; 2; 3; 4; 5|]) 5 "len should return correct array length"
                Expect.equal (len [||]) 0 "len should return 0 for empty array"
            ]
        
        testList "ValueOption Operations" [
            testCase "is_some checks option correctly" <| fun _ ->
                Expect.isTrue (is_some (Some 42)) "is_some should return true for Some"
                Expect.isFalse (is_some None<int>) "is_some should return false for None"
            
            testCase "is_none checks option correctly" <| fun _ ->
                // Test using IsSome/IsNone properties instead of equality
                let noneVal = None<int>
                let someVal = Some 42
                
                Expect.isTrue (is_none noneVal) "is_none should return true for None"
                Expect.isFalse (is_none someVal) "is_none should return false for Some"
            
            testCase "value unwraps option correctly" <| fun _ ->
                Expect.equal (value (Some 42)) 42 "value should return the wrapped value"
                Expect.throws (fun () -> value None<int> |> ignore) "value should throw for None"
            
            testCase "default_with applies fallback for None" <| fun _ ->
                Expect.equal (default_with (fun () -> 99) (Some 42)) 42 "default_with should return value for Some"
                Expect.equal (default_with (fun () -> 99) None<int>) 99 "default_with should return fallback for None"
                
                // Test with side-effects
                let mutable called = false
                let fallback () = called <- true; 99
                
                let _ = default_with fallback (Some 42)
                Expect.isFalse called "default_with shouldn't call fallback for Some"
                
                called <- false
                let _ = default_with fallback None<int>
                Expect.isTrue called "default_with should call fallback for None"
            
            testCase "some wraps value in Some" <| fun _ ->
                // Type annotation for clarity
                let option1: ValueOption<int> = Some 42
                let option2: ValueOption<int> = Some 42
                Expect.isTrue option1.IsSome "Option should have a value"
                Expect.equal option1.Value 42 "Option should contain 42"
            
            testCase "none creates None value" <| fun _ ->
                // Test using properties instead of equality
                let noneVal = None<int>
                
                // Check properties directly
                Expect.isTrue noneVal.IsNone "none should create a value with IsNone=true"
                Expect.isFalse noneVal.IsSome "none should create a value with IsSome=false"
                Expect.isTrue (is_none noneVal) "none should create a value that is_none considers None"
        ]
        
        testList "Equality Operations" [
            testCase "equals compares values correctly" <| fun _ ->
                // Basic values
                Expect.isTrue (equals 42 42) "equals should return true for same ints"
                Expect.isFalse (equals 42 99) "equals should return false for different ints"
                
                // String comparison
                Expect.isTrue (equals "hello" "hello") "equals should return true for same strings"
                Expect.isFalse (equals "hello" "world") "equals should return false for different strings"
                
                // Custom type comparison
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 1.0; Y = 2.0 }
                let v3 = { X = 3.0; Y = 4.0 }
                
                Expect.isTrue (equals v1 v2) "equals should return true for equal vectors"
                Expect.isFalse (equals v1 v3) "equals should return false for different vectors"
            
            testCase "not_equals compares values correctly" <| fun _ ->
                // Basic values
                Expect.isFalse (not_equals 42 42) "not_equals should return false for same ints"
                Expect.isTrue (not_equals 42 99) "not_equals should return true for different ints"
                
                // String comparison
                Expect.isFalse (not_equals "hello" "hello") "not_equals should return false for same strings"
                Expect.isTrue (not_equals "hello" "world") "not_equals should return true for different strings"
                
                // Custom type comparison
                let v1 = { X = 1.0; Y = 2.0 }
                let v2 = { X = 1.0; Y = 2.0 }
                let v3 = { X = 3.0; Y = 4.0 }
                
                Expect.isFalse (not_equals v1 v2) "not_equals should return false for equal vectors"
                Expect.isTrue (not_equals v1 v3) "not_equals should return true for different vectors"
        ]
        
        testList "Type Default Values" [
            testCase "zero provides correct default values" <| fun _ ->
                Expect.equal (zero<int>) 0 "zero<int> should be 0"
                Expect.equal (zero<float>) 0.0 "zero<float> should be 0.0"
                
                let vectorZero = zero<Vector2D>
                Expect.equal vectorZero.X 0.0 "zero<Vector2D> X should be 0.0"
                Expect.equal vectorZero.Y 0.0 "zero<Vector2D> Y should be 0.0"
                
                let personZero = zero<TestPerson>
                Expect.equal personZero.Name "" "zero<TestPerson> Name should be empty"
                Expect.equal personZero.Age 0 "zero<TestPerson> Age should be 0"
                Expect.equal personZero.IsActive false "zero<TestPerson> IsActive should be false"
            
            testCase "one provides correct unit values" <| fun _ ->
                Expect.equal (one<int>) 1 "one<int> should be 1"
                Expect.equal (one<float>) 1.0 "one<float> should be 1.0"
                
                let vectorOne = one<Vector2D>
                Expect.equal vectorOne.X 1.0 "one<Vector2D> X should be 1.0"
                Expect.equal vectorOne.Y 1.0 "one<Vector2D> Y should be 1.0"
                
                let personOne = one<TestPerson>
                Expect.equal personOne.Name "John" "one<TestPerson> Name should be 'John'"
                Expect.equal personOne.Age 1 "one<TestPerson> Age should be 1"
                Expect.equal personOne.IsActive true "one<TestPerson> IsActive should be true"
        ]
        
        testList "String Operations" [
            testCase "string converts values to string representation" <| fun _ ->
                Expect.equal (string 42) "42" "string should convert int to string"
                Expect.equal (string 3.14) "3.14" "string should convert float to string"
                Expect.equal (string true) "True" "string should convert bool to string"
                
                let vector = { X = 1.0; Y = 2.0 }
                Expect.equal (string vector) "{ X = 1.000000; Y = 2.000000 }" "string should convert Vector2D to string"
                
                let person = { Name = "Alice"; Age = 30; IsActive = true }
                // We can only check that string returns something (type-based)
                Expect.isTrue ((string person).Length > 0) "string should convert TestPerson to string"
        ]
    ]

// Register the tests
[<Tests>]
let tests = coreTests