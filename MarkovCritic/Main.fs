module Main

open ParameterParser
open Loader
open Evaluate
open Opinioner

let main argv = 
    let parametersResult = 
        argv
        |> List.ofArray
        |> parseCommandLine
        |> Option.map evaluate
        |> Option.bind Async.RunSynchronously
        |> makeOpinion

   // Critiquer.run
    printfn "%A" parametersResult