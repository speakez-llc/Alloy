module Alloy.Tests.Program

open Expecto

let allTests = testList "All" [
    CoreTests.tests
    NumericTests.tests
    OperatorTests.tests
    ResultTests.tests
    SpanTests.tests
    StringTests.tests
    ValueOptionTests.tests
]

[<EntryPoint>]
let main argv = 
    Tests.runTestsWithCLIArgs [] argv allTests