module Alloy.Tests.Program

open Expecto

// Define the tests list with proper [<Tests>] attribute
[<Tests>]
let allTests = 
    testList "All" [
        CoreTests.tests
        NumericTests.tests
        OperatorTests.tests
        ResultTests.tests
        SpanTests.tests
        StringTests.tests
        ValueOptionTests.tests
    ]

// Entry point with correct Tests module reference
[<EntryPoint>]
let main argv =
    // Create a timeout timer
    let timeout = 30000 // 30 seconds in milliseconds
    let timer = new System.Timers.Timer(float timeout)
    timer.AutoReset <- false
    timer.Elapsed.Add(fun _ -> 
        printfn "Test execution timed out after 30 seconds!"
        printfn "Force exiting process..."
        System.Environment.Exit(1)
    )
    
    // Start the timer
    timer.Start()
    
    // Run tests with verbosity set to show progress
    let debugArgs = Array.append argv [|"--debug"|]
    
    // Use Tests module function - this is the key fix
    let exitCode = Tests.runTestsWithCLIArgs [] debugArgs allTests
    
    // Stop the timer if tests complete normally
    timer.Stop()
    
    // Return the exit code
    exitCode