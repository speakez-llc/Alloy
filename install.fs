// install.fs - A simple script to copy Alloy source files into a project
open System
open System.IO

let sourceDir = Path.Combine(__SOURCE_DIRECTORY__, "src")
let targetDir = 
    match Environment.GetCommandLineArgs() with
    | args when args.Length > 1 -> args.[1]
    | _ -> Path.Combine(Environment.CurrentDirectory, "Alloy")

// Create target directory if it doesn't exist
if not (Directory.Exists targetDir) then
    Directory.CreateDirectory targetDir |> ignore
    printfn "Created directory: %s" targetDir

// Copy all source files
for file in Directory.GetFiles(sourceDir, "*.fs") do
    let fileName = Path.GetFileName(file)
    let targetPath = Path.Combine(targetDir, fileName)
    File.Copy(file, targetPath, true)
    printfn "Copied %s to %s" fileName targetDir

printfn "Alloy source files installed to %s" targetDir