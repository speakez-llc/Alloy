module Alloy.Tests.Program

open Expecto

[<EntryPoint>]
let main argv = 
    runTestsWithCLIArgs [] argv BasicTests.basicTests