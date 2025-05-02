module Alloy.Tests.BasicTests

open Expecto
// Open specific modules explicitly
open Alloy.Core 
open Alloy.Numeric
open Alloy.Operators

[<Tests>]
let basicTests =
    testList "Basic" [
        testCase "Numeric operations" <| fun _ ->
            // Test direct function calls
            let result1 = add 5 3
            Expect.equal result1 8 "Direct addition should work"
            
            let result2 = subtract 10 4
            Expect.equal result2 6 "Direct subtraction should work"
            
            let result3 = multiply 3 7
            Expect.equal result3 21 "Direct multiplication should work"
            
            let result4 = divide 20 5
            Expect.equal result4 4 "Direct division should work"
            
            // Test operators
            let opResult1 = 5 + 3
            Expect.equal opResult1 8 "Addition operator should work"
            
            let opResult2 = 10 - 4
            Expect.equal opResult2 6 "Subtraction operator should work"
            
            let opResult3 = 3 * 7
            Expect.equal opResult3 21 "Multiplication operator should work"
            
            let opResult4 = 20 / 5
            Expect.equal opResult4 4 "Division operator should work"
        
        testCase "Collection operations" <| fun _ ->
            // Test array operations
            let array = [|1; 2; 3; 4; 5|]
            let length = len array
            Expect.equal length 5 "len function should work"
            
            // Test map function
            let doubled = map (fun x -> x * 2) array
            Expect.sequenceEqual doubled [|2; 4; 6; 8; 10|] "map function should work"
            
            // Test sum and average
            let total = sum array
            Expect.equal total 15 "sum function should work"
            
            let avg = average array
            Expect.equal avg 3 "average function should work"
            
            // Test filter function
            let evens = filter (fun x -> x % 2 = 0) array
            Expect.sequenceEqual evens [|2; 4|] "filter function should work"
        
        testCase "Complex expressions" <| fun _ ->
            // Test more complex expressions combining operators
            let result1 = 5 + 3 * 2
            Expect.equal result1 11 "Complex expression 1 should work"
            
            let result2 = (5 + 3) * 2
            Expect.equal result2 16 "Complex expression 2 should work"
            
            // Test with custom computation
            let computation x y = (x + y) * (x - y)
            let result3 = computation 10 4
            Expect.equal result3 84 "Custom computation should work"
    ]