module Alloy.Tests.CollectionTests

open Expecto
open Alloy
open Alloy.Collection

/// Tests for collection operations
let collectionTests =
    testList "Collection" [
        testList "Array Operations" [
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
        
        // You could add similar tests for other collection types (List, ResizeArray, etc.)
        // if your implementation supports them
    ]

// Add tests to the test group
[<Tests>]
let tests = collectionTests