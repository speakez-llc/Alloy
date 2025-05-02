module Alloy.Tests.StringTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for the String module
let stringTests =
    testList "String" [
        // [Other test lists here...]
        
        testList "Practical Scenarios" [
            testCase "Safe string manipulation chain" <| fun _ ->
                // Handle potentially null or empty strings safely
                let input = "  Hello, World!  "
                let result = input |> String.trim |> String.toLower |> String.replace "hello" "hi"
                Expect.equal result "hi, world!" "Chain should work on valid input"
                
                // Same chain with null input
                let nullResult = null |> String.trim |> String.toLower |> String.replace "hello" "hi"
                Expect.equal nullResult "" "Chain should not throw on null input"
            
            testCase "CSV parsing example" <| fun _ ->
                let csv = "Name,Age,Active\nAlice,30,true\nBob,25,false"
                
                // Parse line by line
                let lines = csv |> String.split '\n'
                Expect.equal lines.Length 3 "Should have 3 lines"
                
                // Parse headers
                let headers = lines.[0] |> String.split ','
                Expect.sequenceEqual headers [|"Name"; "Age"; "Active"|] "Headers should match"
                
                // Parse first row
                let row1 = lines.[1] |> String.split ','
                Expect.sequenceEqual row1 [|"Alice"; "30"; "true"|] "First row should match"
                
                // Convert values with safe parsing
                let isActive = 
                    if String.equalsIgnoreCase "true" row1.[2] then true
                    elif String.equalsIgnoreCase "false" row1.[2] then false
                    else false
                
                let age = 
                    if String.isInteger row1.[1] then
                        int row1.[1]
                    else
                        0
                
                Expect.equal age 30 "Age should be parsed correctly"
                Expect.isTrue isActive "IsActive should be parsed correctly"
            
            testCase "URL parsing example" <| fun _ ->
                let url = "https://example.com/path?param1=value1&param2=value2"
                
                // Extract domain
                let domainStart = String.indexOf "://" url + 3
                // Fix: The String.indexOf function in Alloy only takes two parameters
                let domainEnd = String.indexOf "/" url
                let domainEndAfterStart = if domainEnd > domainStart then domainEnd else url.Length
                let domain = String.substring domainStart (domainEndAfterStart - domainStart) url
                Expect.equal domain "example.com" "Domain should be extracted correctly"
                
                // Extract path
                let pathStart = domainEndAfterStart
                let pathEnd = String.indexOf "?" url
                let path = 
                    if pathEnd > 0 then
                        String.substring pathStart (pathEnd - pathStart) url
                    else
                        String.substringFrom pathStart url
                Expect.equal path "/path" "Path should be extracted correctly"
                
                // Extract query parameters
                let queryStart = pathEnd + 1
                let query = String.substringFrom queryStart url
                let paramPairs = String.split '&' query
                
                // Extract first parameter
                let firstParam = paramPairs.[0]
                let keyValueSplit = String.indexOf "=" firstParam
                let key = String.substring 0 keyValueSplit firstParam
                let value = String.substringFrom (keyValueSplit + 1) firstParam
                
                Expect.equal key "param1" "Parameter key should be extracted correctly"
                Expect.equal value "value1" "Parameter value should be extracted correctly"
        ]
    ]

// Register the tests
[<Tests>]
let tests = stringTests