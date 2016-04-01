module Main

open ParameterParser

let main argv = 
    let parametersResult = 
        argv
        |> List.ofArray
        |> parseCommandLine

    Critiquer.run
    parametersResult