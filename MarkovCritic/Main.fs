module Main

open ParameterParser
open Loader
open Evaluate

let main argv = 
    let parametersResult = 
        argv
        |> List.ofArray
        |> parseCommandLine
        |> Option.map evaluate
        |> Option.bind Async.RunSynchronously

   // Critiquer.run
    printfn "%A" parametersResult