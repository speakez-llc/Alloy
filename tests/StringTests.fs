module Alloy.Tests.StringTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for the String module
let stringTests =
    testList "String" [
        // Basic Tests
        testCase "length returns correct string length" <| fun _ ->
            Expect.equal (String.length "hello") 5 "String length should be 5"
            Expect.equal (String.length "") 0 "Empty string length should be 0"
            Expect.equal (String.length null) 0 "Null string length should be 0"
            
        testCase "isEmpty detects empty strings" <| fun _ ->
            Expect.isTrue (String.isEmpty "") "Empty string should be empty"
            Expect.isTrue (String.isEmpty null) "Null string should be empty"
            Expect.isFalse (String.isEmpty "hello") "Non-empty string should not be empty"
            
        testCase "isNullOrEmpty detects null or empty strings" <| fun _ ->
            Expect.isTrue (String.isNullOrEmpty "") "Empty string should be null or empty"
            Expect.isTrue (String.isNullOrEmpty null) "Null string should be null or empty"
            Expect.isFalse (String.isNullOrEmpty "hello") "Non-empty string should not be null or empty"
            
        testCase "trim removes whitespace" <| fun _ ->
            Expect.equal (String.trim "  hello  ") "hello" "Trim should remove whitespace"
            Expect.equal (String.trim "hello") "hello" "Trim should not change strings without whitespace"
            Expect.equal (String.trim null) "" "Trim should handle null strings"
            
        testCase "split divides string correctly" <| fun _ ->
            let result = String.split ',' "a,b,c"
            Expect.equal result.Length 3 "Split should create correct number of parts"
            Expect.equal result.[0] "a" "First part should be correct"
            Expect.equal result.[1] "b" "Second part should be correct"
            Expect.equal result.[2] "c" "Third part should be correct"
            
        testCase "contains detects substrings" <| fun _ ->
            Expect.isTrue (String.contains "lo" "hello") "String should contain substring"
            Expect.isFalse (String.contains "z" "hello") "String should not contain absent substring"
            Expect.isFalse (String.contains "hello" null) "Null string contains nothing"
            
        testCase "toLower converts to lowercase" <| fun _ ->
            Expect.equal (String.toLower "HELLO") "hello" "toLower should convert to lowercase"
            Expect.equal (String.toLower "Hello") "hello" "toLower should convert mixed case"
            Expect.equal (String.toLower null) "" "toLower should handle null strings"
            
        testCase "toUpper converts to uppercase" <| fun _ ->
            Expect.equal (String.toUpper "hello") "HELLO" "toUpper should convert to uppercase"
            Expect.equal (String.toUpper "Hello") "HELLO" "toUpper should convert mixed case"
            Expect.equal (String.toUpper null) "" "toUpper should handle null strings"
            
        testCase "replace substitutes substrings" <| fun _ ->
            Expect.equal (String.replace "l" "L" "hello") "heLLo" "replace should substitute all occurrences"
            Expect.equal (String.replace "z" "Z" "hello") "hello" "replace should do nothing for absent substrings"
            Expect.equal (String.replace "hello" "bye" null) "" "replace should handle null strings"
            
        testCase "startsWith detects prefixes" <| fun _ ->
            Expect.isTrue (String.startsWith "he" "hello") "String should start with prefix"
            Expect.isFalse (String.startsWith "lo" "hello") "String should not start with non-prefix"
            Expect.isFalse (String.startsWith "hello" null) "Null string starts with nothing"
            
        testCase "endsWith detects suffixes" <| fun _ ->
            Expect.isTrue (String.endsWith "lo" "hello") "String should end with suffix"
            Expect.isFalse (String.endsWith "he" "hello") "String should not end with non-suffix"
            Expect.isFalse (String.endsWith "hello" null) "Null string ends with nothing"
            
        // Practical Scenarios
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
            let lines = csv.Split('\n')
            Expect.equal lines.Length 3 "Should have 3 lines"
            
            // Parse headers
            let headers = lines.[0].Split(',')
            Expect.equal headers.[0] "Name" "First header should be Name"
            Expect.equal headers.[1] "Age" "Second header should be Age"
            Expect.equal headers.[2] "Active" "Third header should be Active"
            
            // Parse first row
            let row1 = lines.[1].Split(',')
            Expect.equal row1.[0] "Alice" "First value should be Alice"
            Expect.equal row1.[1] "30" "Second value should be 30"
            Expect.equal row1.[2] "true" "Third value should be true"
            
            // Convert values safely
            let isActive = 
                if String.equalsIgnoreCase "true" row1.[2] then true
                elif String.equalsIgnoreCase "false" row1.[2] then false
                else false
            
            let age = 
                if String.isInteger row1.[1] then
                    int row1.[1]
                else 0
            
            Expect.equal age 30 "Age should be parsed correctly"
            Expect.isTrue isActive "IsActive should be parsed correctly"
            
        testCase "URL parsing example" <| fun _ ->
            let url = "https://example.com/path?param1=value1&param2=value2"
            
            // Extract domain safely
            let protocolStr = "://"
            let protocolIndex = url.IndexOf(protocolStr)
            let domainStart = protocolIndex + protocolStr.Length
            let pathSlash = url.IndexOf('/', domainStart)
            
            // Extract domain
            let domain = url.Substring(domainStart, pathSlash - domainStart)
            Expect.equal domain "example.com" "Domain should be extracted correctly"
            
            // Extract path
            let queryMark = url.IndexOf('?')
            let path = url.Substring(pathSlash, queryMark - pathSlash)
            Expect.equal path "/path" "Path should be extracted correctly"
            
            // Extract query parameters
            let query = url.Substring(queryMark + 1)
            let paramPairs = query.Split('&')
            let firstParam = paramPairs.[0]
            let equalSign = firstParam.IndexOf('=')
            
            let key = firstParam.Substring(0, equalSign)
            let value = firstParam.Substring(equalSign + 1)
            
            Expect.equal key "param1" "Parameter key should be extracted correctly"
            Expect.equal value "value1" "Parameter value should be extracted correctly"
    ]

// Register the tests
[<Tests>]
let tests = stringTests