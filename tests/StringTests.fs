module Alloy.Tests.StringTests

open Expecto
open Alloy

/// Tests for string operations
let stringTests =
    testList "String" [
        testCase "Length - matches System.String.Length" <| fun _ ->
            let testStr = "Hello, world!"
            Expect.equal (String.length testStr) (testStr.Length) "String length should match"
        
        testCase "IsEmpty - matches System.String.IsNullOrEmpty" <| fun _ ->
            let emptyStr = ""
            let nonEmptyStr = "Hello"
            Expect.equal (String.isEmpty emptyStr) (System.String.IsNullOrEmpty(emptyStr)) "Empty string check should match"
            Expect.equal (String.isEmpty nonEmptyStr) (System.String.IsNullOrEmpty(nonEmptyStr)) "Non-empty string check should match"
        
        testCase "IsNullOrEmpty - matches System.String.IsNullOrEmpty" <| fun _ ->
            let nullStr: string = null
            let emptyStr = ""
            let nonEmptyStr = "Hello"
            Expect.equal (String.isNullOrEmpty nullStr) (System.String.IsNullOrEmpty(nullStr)) "Null string check should match"
            Expect.equal (String.isNullOrEmpty emptyStr) (System.String.IsNullOrEmpty(emptyStr)) "Empty string check should match"
            Expect.equal (String.isNullOrEmpty nonEmptyStr) (System.String.IsNullOrEmpty(nonEmptyStr)) "Non-empty string check should match"
        
        testCase "IsWhiteSpace - matches System.String.IsNullOrWhiteSpace" <| fun _ ->
            let whitespaceStr = "   \t\n"
            let nonWhitespaceStr = "Hello"
            Expect.equal (String.isWhiteSpace whitespaceStr) (whitespaceStr.Trim().Length = 0) "Whitespace check should match"
            Expect.equal (String.isWhiteSpace nonWhitespaceStr) (nonWhitespaceStr.Trim().Length = 0) "Non-whitespace check should match"
        
        testCase "IsNullOrWhiteSpace - matches System.String.IsNullOrWhiteSpace" <| fun _ ->
            let nullStr: string = null
            let whitespaceStr = "   \t\n"
            let nonWhitespaceStr = "Hello"
            Expect.equal (String.isNullOrWhiteSpace nullStr) (System.String.IsNullOrWhiteSpace(nullStr)) "Null string check should match"
            Expect.equal (String.isNullOrWhiteSpace whitespaceStr) (System.String.IsNullOrWhiteSpace(whitespaceStr)) "Whitespace string check should match"
            Expect.equal (String.isNullOrWhiteSpace nonWhitespaceStr) (System.String.IsNullOrWhiteSpace(nonWhitespaceStr)) "Non-whitespace string check should match"
        
        testCase "Substring - matches System.String.Substring" <| fun _ ->
            let testStr = "Hello, world!"
            Expect.equal (String.substring 0 5 testStr) (testStr.Substring(0, 5)) "Substring from start should match"
            Expect.equal (String.substring 7 5 testStr) (testStr.Substring(7, 5)) "Substring from middle should match"
        
        testCase "SubstringFrom - matches System.String.Substring" <| fun _ ->
            let testStr = "Hello, world!"
            Expect.equal (String.substringFrom 7 testStr) (testStr.Substring(7)) "Substring from index to end should match"
        
        testCase "Split - matches System.String.Split" <| fun _ ->
            let testStr = "one,two,three"
            let separator = ','
            Expect.sequenceEqual (String.split separator testStr) (testStr.Split(separator)) "Split by single separator should match"
        
        testCase "SplitMany - matches System.String.Split" <| fun _ ->
            let testStr = "one,two;three"
            let separators = [|','; ';'|]
            Expect.sequenceEqual (String.splitMany separators testStr) (testStr.Split(separators)) "Split by multiple separators should match"
        
        testCase "Join - matches System.String.Join" <| fun _ ->
            let parts = [|"one"; "two"; "three"|]
            let separator = ", "
            Expect.equal (String.join separator parts) (System.String.Join(separator, parts)) "Join with separator should match"
        
        testCase "Trim - matches System.String.Trim" <| fun _ ->
            let testStr = "  Hello, world!  \t\n"
            Expect.equal (String.trim testStr) (testStr.Trim()) "Trim should match"
        
        testCase "TrimStart - matches System.String.TrimStart" <| fun _ ->
            let testStr = "  Hello, world!  \t\n"
            Expect.equal (String.trimStart testStr) (testStr.TrimStart()) "TrimStart should match"
        
        testCase "TrimEnd - matches System.String.TrimEnd" <| fun _ ->
            let testStr = "  Hello, world!  \t\n"
            Expect.equal (String.trimEnd testStr) (testStr.TrimEnd()) "TrimEnd should match"
        
        testCase "Replace - matches System.String.Replace" <| fun _ ->
            let testStr = "Hello, world!"
            let oldValue = "world"
            let newValue = "F#"
            Expect.equal (String.replace oldValue newValue testStr) (testStr.Replace(oldValue, newValue)) "Replace substring should match"
        
        testCase "StartsWith - matches System.String.StartsWith" <| fun _ ->
            let testStr = "Hello, world!"
            let prefix1 = "Hello"
            let prefix2 = "world"
            Expect.equal (String.startsWith prefix1 testStr) (testStr.StartsWith(prefix1)) "StartsWith positive case should match"
            Expect.equal (String.startsWith prefix2 testStr) (testStr.StartsWith(prefix2)) "StartsWith negative case should match"
        
        testCase "EndsWith - matches System.String.EndsWith" <| fun _ ->
            let testStr = "Hello, world!"
            let suffix1 = "world!"
            let suffix2 = "Hello"
            Expect.equal (String.endsWith suffix1 testStr) (testStr.EndsWith(suffix1)) "EndsWith positive case should match"
            Expect.equal (String.endsWith suffix2 testStr) (testStr.EndsWith(suffix2)) "EndsWith negative case should match"
        
        testCase "ToLower - matches System.String.ToLower" <| fun _ ->
            let testStr = "Hello, WORLD!"
            Expect.equal (String.toLower testStr) (testStr.ToLower()) "ToLower should match"
        
        testCase "ToUpper - matches System.String.ToUpper" <| fun _ ->
            let testStr = "Hello, world!"
            Expect.equal (String.toUpper testStr) (testStr.ToUpper()) "ToUpper should match"
        
        testCase "IndexOf - matches System.String.IndexOf" <| fun _ ->
            let testStr = "Hello, world!"
            let substring1 = "world"
            let substring2 = "missing"
            Expect.equal (String.indexOf substring1 testStr) (testStr.IndexOf(substring1)) "IndexOf existing substring should match"
            Expect.equal (String.indexOf substring2 testStr) (testStr.IndexOf(substring2)) "IndexOf missing substring should match"
        
        testCase "LastIndexOf - matches System.String.LastIndexOf" <| fun _ ->
            let testStr = "Hello, Hello, world!"
            let substring = "Hello"
            Expect.equal (String.lastIndexOf substring testStr) (testStr.LastIndexOf(substring)) "LastIndexOf should match"
        
        testCase "Contains - matches System.String.Contains" <| fun _ ->
            let testStr = "Hello, world!"
            let substring1 = "world"
            let substring2 = "missing"
            Expect.equal (String.contains substring1 testStr) (testStr.Contains(substring1)) "Contains existing substring should match"
            Expect.equal (String.contains substring2 testStr) (testStr.Contains(substring2)) "Contains missing substring should match"
    ]

// Add tests to the test group
[<Tests>]
let tests = stringTests