module Alloy.Tests.StringTests

open Expecto
open Alloy
open Alloy.Tests.TestHelpers

/// Tests for the String module
let stringTests =
    testList "String" [
        testList "Basic Properties" [
            testCase "length returns correct string length" <| fun _ ->
                Expect.equal (String.length "hello") 5 "length should return correct string length"
                Expect.equal (String.length "") 0 "length should return 0 for empty string"
                Expect.equal (String.length null) 0 "length should return 0 for null string"
            
            testCase "isEmpty returns correct result" <| fun _ ->
                Expect.isTrue (String.isEmpty "") "isEmpty should return true for empty string"
                Expect.isTrue (String.isEmpty null) "isEmpty should return true for null string"
                Expect.isFalse (String.isEmpty "hello") "isEmpty should return false for non-empty string"
            
            testCase "isNullOrEmpty returns correct result" <| fun _ ->
                Expect.isTrue (String.isNullOrEmpty "") "isNullOrEmpty should return true for empty string"
                Expect.isTrue (String.isNullOrEmpty null) "isNullOrEmpty should return true for null string"
                Expect.isFalse (String.isNullOrEmpty "hello") "isNullOrEmpty should return false for non-empty string"
            
            testCase "isNullOrWhiteSpace returns correct result" <| fun _ ->
                Expect.isTrue (String.isNullOrWhiteSpace "") "isNullOrWhiteSpace should return true for empty string"
                Expect.isTrue (String.isNullOrWhiteSpace null) "isNullOrWhiteSpace should return true for null string"
                Expect.isTrue (String.isNullOrWhiteSpace "   \t\n") "isNullOrWhiteSpace should return true for whitespace string"
                Expect.isFalse (String.isNullOrWhiteSpace "hello") "isNullOrWhiteSpace should return false for non-empty string"
                
                // Mixed whitespace and content
                Expect.isFalse (String.isNullOrWhiteSpace "  hello  ") "isNullOrWhiteSpace should return false for string with content"
        ]
        
        testList "String Manipulation" [
            testCase "substring extracts correct substring" <| fun _ ->
                let s = "hello world"
                Expect.equal (String.substring 0 5 s) "hello" "substring should extract from start"
                Expect.equal (String.substring 6 5 s) "world" "substring should extract from middle"
                Expect.equal (String.substring 0 0 s) "" "substring with zero length should return empty"
                Expect.equal (String.substring 20 5 s) "" "substring with invalid start index should return empty"
                Expect.equal (String.substring 5 100 s) " world" "substring with excessive length should return to end"
                Expect.equal (String.substring 0 5 null) "" "substring on null should return empty"
                
                // Negative indices
                Expect.equal (String.substring -1 5 s) "" "substring with negative index should return empty"
            
            testCase "substringFrom extracts to end" <| fun _ ->
                let s = "hello world"
                Expect.equal (String.substringFrom 6 s) "world" "substringFrom should extract from index to end"
                Expect.equal (String.substringFrom 0 s) "hello world" "substringFrom from start should return whole string"
                Expect.equal (String.substringFrom 20 s) "" "substringFrom with invalid index should return empty"
                Expect.equal (String.substringFrom 0 null) "" "substringFrom on null should return empty"
                
                // Negative indices
                Expect.equal (String.substringFrom -1 s) "" "substringFrom with negative index should return empty"
            
            testCase "contains checks for substring presence" <| fun _ ->
                let s = "hello world"
                Expect.isTrue (String.contains "world" s) "contains should return true for present substring"
                Expect.isTrue (String.contains "hello" s) "contains should return true for substring at start"
                Expect.isFalse (String.contains "goodbye" s) "contains should return false for absent substring"
                Expect.isFalse (String.contains "world" null) "contains should return false for null string"
                Expect.isFalse (String.contains null s) "contains should return false for null search"
                
                // Case sensitivity
                Expect.isFalse (String.contains "HELLO" s) "contains should be case-sensitive"
            
            testCase "split divides string correctly" <| fun _ ->
                let s = "one,two,three"
                let result = String.split ',' s
                Expect.sequenceEqual result [|"one"; "two"; "three"|] "split should correctly divide string"
                
                let emptyResult = String.split ',' ""
                Expect.sequenceEqual emptyResult [|""|] "split on empty string should return array with one empty string"
                
                let nullResult = String.split ',' null
                Expect.sequenceEqual nullResult [||] "split on null should return empty array"
                
                // With multiple delimiters
                let multiDelim = "one,two,three,,four,"
                let multiResult = String.split ',' multiDelim
                Expect.sequenceEqual multiResult [|"one"; "two"; "three"; ""; "four"; ""|] "split should handle consecutive delimiters"
            
            testCase "splitMany divides string with multiple separators" <| fun _ ->
                let s = "one,two;three.four"
                let result = String.splitMany [|','; ';'; '.'|] s
                Expect.sequenceEqual result [|"one"; "two"; "three"; "four"|] "splitMany should correctly divide string"
                
                let emptyResult = String.splitMany [|','; ';'|] ""
                Expect.sequenceEqual emptyResult [|""|] "splitMany on empty string should return array with one empty string"
                
                let nullResult = String.splitMany [|','; ';'|] null
                Expect.sequenceEqual nullResult [||] "splitMany on null should return empty array"
                
                // With multiple consecutive delimiters
                let multiDelim = "one,;two,.three"
                let multiResult = String.splitMany [|','; ';'; '.'|] multiDelim
                Expect.sequenceEqual multiResult [|"one"; ""; "two"; ""; "three"|] "splitMany should handle consecutive delimiters"
            
            testCase "join combines strings with separator" <| fun _ ->
                let parts = [|"one"; "two"; "three"|]
                let result = String.join ", " parts
                Expect.equal result "one, two, three" "join should combine strings with separator"
                
                let emptyResult = String.join ", " [||]
                Expect.equal emptyResult "" "join on empty array should return empty string"
                
                // With empty parts
                let mixedParts = [|"one"; ""; "three"|]
                let mixedResult = String.join ", " mixedParts
                Expect.equal mixedResult "one, , three" "join should handle empty parts"
            
            testCase "trim removes whitespace" <| fun _ ->
                let s = "  hello world  \t\n"
                Expect.equal (String.trim s) "hello world" "trim should remove all whitespace"
                Expect.equal (String.trim "hello world") "hello world" "trim should not affect string without whitespace"
                Expect.equal (String.trim "") "" "trim should not affect empty string"
                Expect.equal (String.trim null) "" "trim on null should return empty string"
                
                // All whitespace
                Expect.equal (String.trim "   \t\n") "" "trim should return empty for all-whitespace string"
            
            testCase "trimStart removes leading whitespace" <| fun _ ->
                let s = "  hello world  \t\n"
                Expect.equal (String.trimStart s) "hello world  \t\n" "trimStart should remove leading whitespace"
                Expect.equal (String.trimStart "hello world") "hello world" "trimStart should not affect string without leading whitespace"
                Expect.equal (String.trimStart "") "" "trimStart should not affect empty string"
                Expect.equal (String.trimStart null) "" "trimStart on null should return empty string"
                
                // All whitespace
                Expect.equal (String.trimStart "   \t\n") "" "trimStart should return empty for all-whitespace string"
            
            testCase "trimEnd removes trailing whitespace" <| fun _ ->
                let s = "  hello world  \t\n"
                Expect.equal (String.trimEnd s) "  hello world" "trimEnd should remove trailing whitespace"
                Expect.equal (String.trimEnd "hello world") "hello world" "trimEnd should not affect string without trailing whitespace"
                Expect.equal (String.trimEnd "") "" "trimEnd should not affect empty string"
                Expect.equal (String.trimEnd null) "" "trimEnd on null should return empty string"
                
                // All whitespace
                Expect.equal (String.trimEnd "   \t\n") "" "trimEnd should return empty for all-whitespace string"
            
            testCase "replace substitutes substring" <| fun _ ->
                let s = "hello world"
                Expect.equal (String.replace "world" "universe" s) "hello universe" "replace should substitute substring"
                Expect.equal (String.replace "goodbye" "hi" s) "hello world" "replace should not affect string without match"
                Expect.equal (String.replace "world" "" s) "hello " "replace with empty should remove substring"
                Expect.equal (String.replace "world" null s) "hello " "replace with null should be like empty string"
                Expect.equal (String.replace null "universe" s) "hello world" "replace null should not change string"
                Expect.equal (String.replace "world" "universe" null) "" "replace on null should return empty string"
                
                // Multiple occurrences
                let multi = "hello hello hello"
                Expect.equal (String.replace "hello" "hi" multi) "hi hi hi" "replace should replace all occurrences"
            
            testCase "startsWith checks prefix" <| fun _ ->
                let s = "hello world"
                Expect.isTrue (String.startsWith "hello" s) "startsWith should return true for matching prefix"
                Expect.isFalse (String.startsWith "world" s) "startsWith should return false for non-prefix substring"
                Expect.isFalse (String.startsWith "goodbye" s) "startsWith should return false for non-matching string"
                Expect.isFalse (String.startsWith "hello" null) "startsWith should return false for null string"
                Expect.isFalse (String.startsWith null s) "startsWith should return false for null prefix"
                
                // Case sensitivity
                Expect.isFalse (String.startsWith "HELLO" s) "startsWith should be case-sensitive"
            
            testCase "endsWith checks suffix" <| fun _ ->
                let s = "hello world"
                Expect.isTrue (String.endsWith "world" s) "endsWith should return true for matching suffix"
                Expect.isFalse (String.endsWith "hello" s) "endsWith should return false for non-suffix substring"
                Expect.isFalse (String.endsWith "universe" s) "endsWith should return false for non-matching string"
                Expect.isFalse (String.endsWith "world" null) "endsWith should return false for null string"
                Expect.isFalse (String.endsWith null s) "endsWith should return false for null suffix"
                
                // Case sensitivity
                Expect.isFalse (String.endsWith "WORLD" s) "endsWith should be case-sensitive"
        ]
        
        testList "Case Conversion" [
            testCase "toLower converts to lowercase" <| fun _ ->
                let s = "Hello WORLD"
                Expect.equal (String.toLower s) "hello world" "toLower should convert to lowercase"
                Expect.equal (String.toLower null) "" "toLower on null should return empty string"
                
                // Already lowercase
                Expect.equal (String.toLower "hello world") "hello world" "toLower should not change already lowercase string"
                
                // Mixed case
                Expect.equal (String.toLower "HeLLo WoRLd") "hello world" "toLower should convert mixed case"
            
            testCase "toUpper converts to uppercase" <| fun _ ->
                let s = "Hello world"
                Expect.equal (String.toUpper s) "HELLO WORLD" "toUpper should convert to uppercase"
                Expect.equal (String.toUpper null) "" "toUpper on null should return empty string"
                
                // Already uppercase
                Expect.equal (String.toUpper "HELLO WORLD") "HELLO WORLD" "toUpper should not change already uppercase string"
                
                // Mixed case
                Expect.equal (String.toUpper "HeLLo WoRLd") "HELLO WORLD" "toUpper should convert mixed case"
        ]
        
        testList "Search Functions" [
            testCase "indexOf finds substring position" <| fun _ ->
                let s = "hello world"
                Expect.equal (String.indexOf "world" s) 6 "indexOf should find substring position"
                Expect.equal (String.indexOf "hello" s) 0 "indexOf should find substring at start"
                Expect.equal (String.indexOf "goodbye" s) -1 "indexOf should return -1 for missing substring"
                Expect.equal (String.indexOf "world" null) -1 "indexOf should return -1 for null string"
                Expect.equal (String.indexOf null s) -1 "indexOf should return -1 for null search"
                
                // Case sensitivity
                Expect.equal (String.indexOf "WORLD" s) -1 "indexOf should be case-sensitive"
                
                // Multiple occurrences
                let multi = "hello world hello"
                Expect.equal (String.indexOf "hello" multi) 0 "indexOf should find first occurrence"
            
            testCase "lastIndexOf finds last occurrence" <| fun _ ->
                let s = "hello world hello"
                Expect.equal (String.lastIndexOf "hello" s) 12 "lastIndexOf should find last occurrence"
                Expect.equal (String.lastIndexOf "world" s) 6 "lastIndexOf should find single occurrence"
                Expect.equal (String.lastIndexOf "goodbye" s) -1 "lastIndexOf should return -1 for missing substring"
                Expect.equal (String.lastIndexOf "hello" null) -1 "lastIndexOf should return -1 for null string"
                Expect.equal (String.lastIndexOf null s) -1 "lastIndexOf should return -1 for null search"
                
                // Case sensitivity
                Expect.equal (String.lastIndexOf "HELLO" s) -1 "lastIndexOf should be case-sensitive"
            
            testCase "indexOfAny finds any character position" <| fun _ ->
                let s = "hello world"
                Expect.equal (String.indexOfAny [|'w'; 'z'|] s) 6 "indexOfAny should find first matching character"
                Expect.equal (String.indexOfAny [|'x'; 'y'; 'z'|] s) -1 "indexOfAny should return -1 for no match"
                Expect.equal (String.indexOfAny [|'h'; 'e'|] s) 0 "indexOfAny should find match at start"
                Expect.equal (String.indexOfAny [|'w'|] null) -1 "indexOfAny should return -1 for null string"
                Expect.equal (String.indexOfAny null s) -1 "indexOfAny should return -1 for null search"
                
                // Multiple occurrences
                Expect.equal (String.indexOfAny [|'l'; 'o'|] s) 2 "indexOfAny should find first occurrence of any character"
        ]
        
        testList "Conversion Functions" [
            testCase "toCharArray converts to char array" <| fun _ ->
                let s = "hello"
                let result = String.toCharArray s
                Expect.sequenceEqual result [|'h'; 'e'; 'l'; 'l'; 'o'|] "toCharArray should convert string to char array"
                
                let emptyResult = String.toCharArray ""
                Expect.sequenceEqual emptyResult [||] "toCharArray should return empty array for empty string"
                
                let nullResult = String.toCharArray null
                Expect.sequenceEqual nullResult [||] "toCharArray should return empty array for null string"
            
            testCase "equalsIgnoreCase compares ignoring case" <| fun _ ->
                let s = "Hello World"
                Expect.isTrue (String.equalsIgnoreCase "hello world" s) "equalsIgnoreCase should return true for case-insensitive match"
                Expect.isTrue (String.equalsIgnoreCase "HELLO WORLD" s) "equalsIgnoreCase should return true for uppercase match"
                Expect.isFalse (String.equalsIgnoreCase "hello universe" s) "equalsIgnoreCase should return false for different strings"
                Expect.isFalse (String.equalsIgnoreCase "hello world" null) "equalsIgnoreCase should return false for null string"
                Expect.isFalse (String.equalsIgnoreCase null s) "equalsIgnoreCase should return false for null comparison"
                Expect.isTrue (String.equalsIgnoreCase null null) "equalsIgnoreCase should return true for both null"
                
                // Mixed case
                Expect.isTrue (String.equalsIgnoreCase "hElLo WoRlD" s) "equalsIgnoreCase should match mixed case"
        ]
        
        testList "Type Checking" [
            testCase "isInteger validates integer strings" <| fun _ ->
                Expect.isTrue (String.isInteger "123") "isInteger should return true for valid integer"
                Expect.isTrue (String.isInteger "-123") "isInteger should return true for negative integer"
                Expect.isFalse (String.isInteger "123.45") "isInteger should return false for decimal"
                Expect.isFalse (String.isInteger "abc") "isInteger should return false for non-numeric"
                Expect.isFalse (String.isInteger "") "isInteger should return false for empty string"
                Expect.isFalse (String.isInteger null) "isInteger should return false for null string"
                
                // Edge cases
                Expect.isTrue (String.isInteger "0") "isInteger should return true for zero"
                Expect.isFalse (String.isInteger "+123") "isInteger should return false for explicit positive"
                Expect.isFalse (String.isInteger " 123") "isInteger should return false for string with whitespace"
            
            testCase "isNumber validates numeric strings" <| fun _ ->
                Expect.isTrue (String.isNumber "123") "isNumber should return true for integer"
                Expect.isTrue (String.isNumber "-123") "isNumber should return true for negative integer"
                Expect.isTrue (String.isNumber "123.45") "isNumber should return true for decimal"
                Expect.isFalse (String.isNumber "abc") "isNumber should return false for non-numeric"
                Expect.isFalse (String.isNumber "") "isNumber should return false for empty string"
                Expect.isFalse (String.isNumber null) "isNumber should return false for null string"
                
                // Edge cases
                Expect.isTrue (String.isNumber "0") "isNumber should return true for zero"
                Expect.isTrue (String.isNumber "0.0") "isNumber should return true for zero decimal"
                Expect.isTrue (String.isNumber "-0.5") "isNumber should return true for negative decimal"
                Expect.isFalse (String.isNumber " 123.45") "isNumber should return false for string with whitespace"
            
            testCase "isDigitsOnly validates digit-only strings" <| fun _ ->
                Expect.isTrue (String.isDigitsOnly "123") "isDigitsOnly should return true for digits only"
                Expect.isFalse (String.isDigitsOnly "-123") "isDigitsOnly should return false for negative"
                Expect.isFalse (String.isDigitsOnly "123.45") "isDigitsOnly should return false for decimal"
                Expect.isFalse (String.isDigitsOnly "abc123") "isDigitsOnly should return false for alphanumeric"
                Expect.isFalse (String.isDigitsOnly "") "isDigitsOnly should return false for empty string"
                Expect.isFalse (String.isDigitsOnly null) "isDigitsOnly should return false for null string"
                
                // Edge cases
                Expect.isTrue (String.isDigitsOnly "0") "isDigitsOnly should return true for zero"
                Expect.isFalse (String.isDigitsOnly " 123") "isDigitsOnly should return false for string with whitespace"
        ]
        
        testList "Concat and CharAt" [
            testCase "concat joins two strings" <| fun _ ->
                Expect.equal (String.concat "hello" "world") "helloworld" "concat should join two strings"
                Expect.equal (String.concat "hello" "") "hello" "concat with empty should return first string"
                Expect.equal (String.concat "" "world") "world" "concat with empty should return second string"
                Expect.equal (String.concat null "world") "world" "concat with null should treat as empty"
                Expect.equal (String.concat "hello" null) "hello" "concat with null should treat as empty"
                Expect.equal (String.concat null null) "" "concat with both null should return empty"
            
            testCase "charAt returns character at position" <| fun _ ->
                let s = "hello"
                
                // Valid indices
                let char0 = String.charAt 0 s
                Expect.isTrue char0.IsSome "charAt should return Some for valid index"
                Expect.equal char0.Value 'h' "charAt should return correct character at start"
                
                let char4 = String.charAt 4 s
                Expect.isTrue char4.IsSome "charAt should return Some for valid index"
                Expect.equal char4.Value 'o' "charAt should return correct character at end"
                
                // Invalid indices
                let charOutOfBounds = String.charAt 5 s
                Expect.isTrue charOutOfBounds.IsNone "charAt should return None for out-of-bounds index"
                
                let charNegative = String.charAt -1 s
                Expect.isTrue charNegative.IsNone "charAt should return None for negative index"
                
                // Null string
                let charNull = String.charAt 0 null
                Expect.isTrue charNull.IsNone "charAt should return None for null string"
        ]
        
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
                let domainEnd = String.indexOf "/" url domainStart
                let domain = String.substring domainStart (domainEnd - domainStart) url
                Expect.equal domain "example.com" "Domain should be extracted correctly"
                
                // Extract path
                let pathStart = domainEnd
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